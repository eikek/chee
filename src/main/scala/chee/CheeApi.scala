package chee

import better.files._
import CheeApi._
import chee.conf._
import chee.cli.TransparentDecrypt
import chee.metadata.MetadataFile
import chee.properties._
import chee.query.{ FileBackend, Index, Progress, SqliteBackend }
import chee.util.files._
import chee.util.more._
import FileOps.{CryptSettings, Result}
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import java.time.Duration
import scala.util.{ Failure, Success, Try }

trait CheeApi {

  def addFiles[C](zero: C, progress: Progress[Result, C])(param: AddParam): C

  def syncFiles[C](zero: C, progress: Progress[Result, C])(param: SyncParam): C

  def importFiles[C](zero: C, progress: Progress[Result, C])(param: ImportParam): C

  def moveFiles[C](zero: C, progress: Progress[Result, C])(param: MoveParam): C

  def getLocationInfo: Try[LocationInfo]

}

object CheeApi {

  def apply(cfg: Config): CheeApi = new CheeApiImpl(cfg)

  type CheeApi = chee.CheeApi
  val CheeApi = chee.CheeApi
  type LocationInfo = Index.LocationInfo
  type Result = FileOps.Result

  case class FileSettings(recursive: Boolean = false, all: Boolean = false, query: String = "", skip: Option[Int] = None, first: Option[Int] = None)
  case class PubkeySecret(keyFile: File, keyPass: Array[Char])
  case class DecryptSettings(pubSecret: Option[PubkeySecret], passphrase: Option[Array[Char]])
  object DecryptSettings {
    def apply(ps: PubkeySecret): DecryptSettings = DecryptSettings(Some(ps), None)
    def apply(p: Array[Char]): DecryptSettings = DecryptSettings(None, Some(p))
    def apply(ps: PubkeySecret, p: Array[Char]): DecryptSettings = DecryptSettings(Some(ps), Some(p))
    val none = DecryptSettings(None, None)
  }


  case class AddParam(find: FileSettings, decrypt: DecryptSettings, failFast: Boolean, files: Seq[File])

  case class SyncParam(find: FileSettings, decrypt: DecryptSettings, failFast: Boolean, reindex: Boolean, all: Boolean, files: Seq[File])

  case class ImportParam(find: FileSettings, decrypt: DecryptSettings, failFast: Boolean, targetDir: File, duplicates: Boolean, files: Seq[File])

  case class MoveParam(source: Seq[File], target: File, indexOnly: Boolean)
}


private class CheeApiImpl(cfg: Config) extends CheeApi with LazyLogging {
  private val index: Index = new SqliteBackend(cfg)

  def getLocationInfo: Try[LocationInfo] =
    index.locationInfo()

  def syncFiles[C](zero: C, progress: Progress[Result, C])(param: SyncParam): C = {
    checkRepoRoot(cfg, param.files)
    val input = if (param.all) index.listLocations().get else param.files
    input.withFilter(!_.exists).map(_.pathAsString).toList match {
      case Nil =>
      case x => UserError(s"""Non existing files: ${x.mkString(", ")}. Use `rm' command if you want to delete everything""")
    }
    val meta = cfg.getMetadataFile
    val syncProgr = progress.setDone(Progress.empty.done)
    val syncAction = FileOps.syncWithIndex(index, DateTime.now, toCryptSettings(param.decrypt, meta), param.reindex)
      .map(unwrap(param.failFast))

    val files: Stream[LazyMap] = fileStream(index, param.find, meta, input)
    val (c, d) = syncProgr.foreach(zero)(files, syncAction)

    val c2 = deleteNonExistingFiles(c, d, progress)(input, param.failFast)

    updateLocation(input)
    c2
  }

  def deleteNonExistingFiles[C](zero: C, dur: Duration, progress: Progress[Result, C])(paths: Seq[File], failFast: Boolean): C = {
    val deleteAction = FileOps.deleteNonExistingFile(index).map(unwrap(failFast))
    val deleteProgr = progress.setBefore(Progress.empty.before).setAfter { (c, t, d) =>
      if (t == Result.Skipped) MapGet.unit(c) // don't call progress for skipped entries
      else progress.before(c).flatMap(_ => progress.after(c, t, d))
    }
    val deleteCond = Condition.or(paths.map(f => Prop(Comp.Like, Ident.path -> s"${f.pathAsString}*")): _*)
    val (c, _) = deleteProgr.foreach(zero, dur)(index.find(deleteCond).get, deleteAction)
    c
  }

  def addFiles[C](zero: C, progress: Progress[Result, C])(param: AddParam): C = {
    checkRepoRoot(cfg, param.files)
    val added = DateTime.now
    val meta = cfg.getMetadataFile
    val addAction = FileOps.addToIndex(index, added, toCryptSettings(param.decrypt, meta))
      .map(unwrap(param.failFast))

    val files: Stream[LazyMap] = fileStream(index, param.find, meta, param.files)
    val (c, _) = progress.foreach(zero)(files, addAction)
    updateLocation(param.files)
    c
  }

  def importFiles[C](zero: C, progress: Progress[Result, C])(param: ImportParam): C = {
    checkRepoRoot(cfg, Seq(param.targetDir))
    val meta = cfg.getMetadataFile
    val importAction = FileOps.importFile(
      index,
      DateTime.now,
      toCryptSettings(param.decrypt, meta),
      param.targetDir,
      param.duplicates).map(unwrap(param.failFast))
    val files: Stream[LazyMap] = fileStream(index, param.find, meta, param.files)
    val (c, _) = progress.foreach(zero)(files, importAction)
    updateLocation(Seq(param.targetDir))
    c
  }

  def moveFiles[C](zero: C, progress: Progress[Result, C])(param: MoveParam): C = {
    param.source.filter(_.notExists) match {
      case x if x.nonEmpty =>
        UserError(s"""The following sources do not exist: ${x.map(_.path).mkString(", ")}""")
      case _ =>
    }
    val isIndexed: File => Boolean =
      f => index.exists(Prop(Comp.Like, Ident.path -> s"${f.pathAsString}*")).get
    param.source.filterNot(isIndexed) match {
      case x if x.nonEmpty =>
        UserError(s"""The following sources are not indexed: ${x.map(_.path).mkString(", ")}""")
      case _ =>
    }
    param.target match {
      case t if !t.exists =>
        param.target.parent.createDirectories()
        t
      case t if !t.isDirectory =>
        UserError("The target must be a directory or should not exist.")
      case _ =>
    }
    val moveAction = moveFile(param.target, param.indexOnly, getLocationFor)_
    val (r, d) = Timing.timedResult(param.source.foldLeft(zero) { (c, src) =>
      val m = progress.before(c).flatMap { _ =>
        val (result, d) = Timing.timedResult(moveAction(src))
        progress.after(c, result, d)
      }
      m.result(LazyMap(
        Ident("source") -> src.pathAsString,
        Ident("target") -> param.target.pathAsString
      ))
    })
    progress.done(r, d)
    r
  }

  private def moveFile(target: File, indexOnly: Boolean, locationFor: File => File)(source: File): Result = {
    val t = target match {
      case Directory(dir) =>
        val f = dir / source.name
        if (f.exists) UserError(s"The target ${f.path} already exists.")
        else f
      case f => f
    }

    val targetLoc = locationFor(t)
    val n = index.move(source.path, t.path, Some(targetLoc.path)).get
    if (!indexOnly) {
      source.moveTo(t)
    }
    updateLocation(Seq(targetLoc))
    Result.Updated
  }

  def checkRepoRoot(conf: Config, dirs: Seq[File]): Unit =
    conf.getRepoRoot match {
      case Some(root) if conf.getBoolean("chee.repo.restrict-to-root") =>
        val errors = dirs.filterNot(_ childOf root)
        if (errors.nonEmpty) {
          UserError(s"""Directories ${errors.mkString(", ")} outside of repository root ${root.path}!""")
        }
      case _ =>
    }

  def updateLocation(paths: Seq[File]): Try[Int] = {
    val data = paths.
      map(f => if (!f.isDirectory) f.parent else f).
      map(p => LazyMap(Ident.location -> p.pathAsString))
    val param = Index.UpdateParam(
      columns = MapGet.unit(Seq(Ident.location)),
      where = MapGet.valueForce(Ident.location).map(p => Prop(Comp.Like, Ident.location -> s"${p}*")))
    index.update(data, param, 0, Progress.count[Boolean])
  }

  def fileStream(index: Index, find: FileSettings, meta: MetadataFile, files: Seq[File]): Stream[LazyMap] = {
    val locationFor = getLocationFor

    val maps = files.toStream.flatMap {
      case Directory(dir) =>
        val content = FileBackend.find(fileCondition(find), dir, find.recursive, meta)
        content.map(_ + (Ident.location -> locationFor(dir).pathAsString))
      case file =>
        Stream(LazyMap.fromFile(file) + (Ident.location -> locationFor(file.parent).pathAsString))
    }
    sliced(find.first, find.skip)(maps).toStream
  }

  def fileCondition(opts: FileSettings): Condition = {
    getFileCondition(opts.query, opts.all) match {
      case Right(cond) => cond
      case Left(msg) => chee.UserError(msg)
    }
  }

  def getFileCondition(query: String, all: Boolean): Either[String, Condition] = {
    val q = cfg.makeQuery
    val defquery =
      if (all) Right(TrueCondition)
      else cfg.fileDefaultQuery(q)

    if (query.trim.isEmpty) defquery
    else for {
      q1 <- defquery.right
      q2 <- q(query.trim).right
    } yield Condition.and(q1, q2)
  }



  private def getLocationFor: File => File = {
    lazy val locations = index.listLocations().get.sortBy(- _.pathAsString.size)
    f => locations.find(_ parentOf f) getOrElse f
  }

  private def unwrap(failFast: Boolean)(t: Try[Result]): Result = t match {
    case Success(r) => r
    case Failure(ex) =>
      if (failFast) throw ex
      else {
        logger.error("Error adding file", ex)
        Result.Error(ex.getMessage)
      }
  }

  private def toCryptSettings(decrypt: DecryptSettings, mf: MetadataFile) =
    (decrypt.pubSecret, decrypt.passphrase) match {
      case (None, None) => None
      case (a, b) => Some(CryptSettings(a, b, TransparentDecrypt.tempDecrypt(cfg), mf))
    }
}
