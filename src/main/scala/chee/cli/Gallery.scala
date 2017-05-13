package chee.cli

import java.io.BufferedOutputStream
import java.util.concurrent.atomic.AtomicReference
import java.util.zip.ZipOutputStream

import CryptOptions.{Opts => CryptOpts}
import Gallery._
import LsOptions.{Opts => LsOpts}
import ProcessingOptions.{Opts => ProcOpts}
import better.files.File
import chee.{Size, OS}
import chee.conf._
import chee.doc.CheeDocInfo
import chee.properties.{FormatPatterns, Ident, LazyMap, MapGet, Property, VirtualProperty}
import chee.properties.MapGet._
import chee.resources.ResourceInfo
import chee.util.files._
import yamusca.imports._
import yamusca.context.{MapValue, Find => ContextFind}
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

class Gallery extends ScoptCommand with AbstractLs with TransparentDecrypt {

  type T = Opts
  val name = "gallery"
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions(_ updateLs _)
    addDecryptOptions(_ updateCrypt _, enableOpt = true)

    noteW("\nGallery options")
    concurrent((c, f) => c.copy(concurrent = true))

    opt[Size]("thumb-size") valueName("<width>x<height>") action { (s, c) =>
      c.copy(size = s)
    } text ("The size for thumbnails. Default is 100x100.")

    opt[Double]("scale-factor") action { (m, c) =>
      c.copy(factor = Some(m))
    } text ("Scale images by this factor.")

    opt[Int]("scale-maxlen") action { (n, c) =>
      c.copy(maxLen = Some(n))
    } textW ("Scale image such that the longest side of the image is not more than `maxlen'. Default is set to 1400.")

    opt[Unit]("no-scale") action { (_, c) =>
      c.copy(maxLen = None, factor = None)
    } textW ("Don't scale images and use original files.")

    opt[Unit]("link-original") action { (_, c) =>
      c.copy(linkOriginal = true)
    } textW("Provide links to the original picture. This requires to include all original files into the gallery folder.")

    opt[File]("template") valueName("<file>") action { (f, c) =>
      mustache.parse(f.contentAsString) match {
        case Right(t) => c.copy(template = Some(t))
        case Left(msg) => userError(msg.toString)
      }
    } textW("A custom mustache template for creating the gallery page.")

    opt[Unit]("write-template") action { (_, c) =>
      c.copy(writeTemplate = true)
    } textW ("Write the default template to current directory or to the file specified by `--out'. "+
      "If this option is set, only the template is written and nothing else is done.")

    opt[String]("theme") action { (s, c) =>
      c.copy(theme = s)
    } textW ("Choose a css theme. Based on bootstrap this selects a css available from bootswatch. "+
      "Possbible themes are: " + (ResourceInfo.categories - "all").mkString(", ") + ". Default is `bootstrap'.")

    opt[String]("title") action { (t, c) =>
      c.copy(title = Some(t))
    } textW("A custom title for the page.")

    opt[File]("out") valueName("<dir or zip>") action { (f, c) =>
      c.copy(out = Some(f))
    } textW("Write the gallery into the given directory. It is created if non existing. "+
      "If the name ends with `.zip' a temporary folder is used and then zipped into a file "+
      "with this name (it must not exist).")

    addQuery(_ updateLs _)

    checkConfig { cfg =>
      val themes = ResourceInfo.categories - "all"
      if (themes(cfg.theme)) success
      else failure(s"${cfg.theme} is not a known theme")
    }
  }

  def processingAction(cfg: Config, opts: Opts, zip: Option[ZipArchive], entries: AtomicReference[Set[String]]): MapGet[Boolean] =
    opts.out match {
      case None => scaleAction(cfg, opts)
      case Some(dir) =>
        def moveFiles(id: Ident, subdir: String, unique: Boolean): MapGet[Boolean] =
          zip match {
            case None => copyAction(id, subdir, dir, unique = false)
            case Some(z) => zipAction(id, subdir, z, unique = false, entries)
          }
        scaleAction(cfg, opts).flatMap {
          case true =>
            moveFiles(thumbnail, "thumbnails", unique = false)
              .flatMap({ _ =>
                if (opts.noScale) unit(true)
                else moveFiles(scaledImage, "images", unique = false)
              })
              .flatMap({ _ =>
                if (!opts.linkOriginal && !opts.noScale) unit(true)
                else moveFiles(Ident.path, "originals", unique = false)
              })
          case false =>
            unit(false)
        }
    }

  def writeTemplate(opts: Opts): Unit = {
    val file = opts.out.getOrElse(File("gallery-template.mustache")) match {
      case Directory(f) => f / "gallery-template.mustache"
      case f => f
    }
    file < ResourceInfo.galleryTemplate.contentAsString
    outln(s"Template written: ${file.path}.")
  }

  def findFiles(cfg: Config, opts: Opts, zip: Option[ZipArchive], entries: AtomicReference[Set[String]]): Stream[LazyMap] = {
    val progress: MapGet[Unit] = valueForce(Ident.filename).map { name =>
      if (opts.concurrent) out(".")
      else outln(s"Processing $name …")
    }
    val action = progress.flatMap(_ => processingAction(cfg, opts, zip, entries))

    val lsOpts = opts.lsOpts.appendQuery(cfg.getString("chee.queries.gallery-default"))
    val files = findDecrypt(cfg, lsOpts, opts.cryptOpts)
    if (opts.concurrent) MapGet.parfilter(files, action)
    else MapGet.filter(files, action)
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    val entries = new AtomicReference[Set[String]](Set.empty)
    if (opts.writeTemplate) writeTemplate(opts)
    else {
      val out: Either[ZipArchive, Option[File]] = opts.out match {
        case Some(dir) if dir.getExtension == Some("zip") =>
          outln(s"Creating zip file ${dir.name} …")
          Left(new ZipArchive(new ZipOutputStream(new BufferedOutputStream(dir.newOutputStream))))
        case _ => Right(opts.out)
      }
      outln("Copy resources…")
      val assets = installAssets(cfg, opts.theme, out)

      outln("Create thumbnails and scale images…")
      val files = findFiles(cfg, opts, out.left.toOption, entries)
      val context = assets :: Context(
        "files" -> Value.fromSeq(files.map(makeContext.result)),
        "title" -> Value.of(opts.title),
        "link-original" -> Value.of(opts.linkOriginal),
        "projectInfo" -> Value.map(
          "version" -> Value.of(CheeDocInfo.version),
          "name" -> Value.of(CheeDocInfo.projectName),
          "homepage" -> Value.of(CheeDocInfo.homepage)
        )
      )

      val html = generateHtml(cfg, opts, context)
      out.left.foreach { z =>
        z.add(html, "index.html")
        z.close()
        html.delete()
      }
      opts.out match {
        case None =>
          OS(cfg).browse(html)
        case Some(f) =>
          if (opts.concurrent) outln("")
          outln(s"Gallery is at ${f.path}")
      }
    }
  }
}

object Gallery {

  val thumbnail: Ident = 'thumbnail
  val scaledImage: Ident = Ident("scaled-image")

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    cryptOpts: CryptOpts = CryptOpts(),
    concurrent: Boolean = false,
    size: Size = Size(100),
    maxLen: Option[Int] = Some(1400),
    factor: Option[Double] = None,
    linkOriginal: Boolean = false,
    template: Option[Template] = None,
    theme: String = "bootstrap",
    title: Option[String] = None,
    writeTemplate: Boolean = false,
    out: Option[File] = None
  ) {
    def updateLs(f: LsOpts => LsOpts) = copy(lsOpts = f(lsOpts))
    def updateCrypt(f: CryptOpts => CryptOpts) = copy(cryptOpts = f(cryptOpts))

    lazy val noScale = maxLen == None && factor == None
  }

  def installAssets(cfg: Config, theme: String, out: Either[ZipArchive, Option[File]]): Context = {
    val root: Either[ZipArchive, File] = out.right.map(_ getOrElse (cfg.getFile("chee.tmpdir") / "gallery"))
    val files = for ((kind, urls) <- ResourceInfo.allOf(theme)) yield {
      root.right.foreach(r => (r / "resources" / kind.name).createDirectories())
      val files = urls.map { u =>
        root.fold(
          zip => {
            val name = s"resources/${kind.name}/${u.fileName}"
            zip.add(u, name)
            name
          },
          root => {
            val target = u downloadIn (root / "resources" / kind.name, overwrite = true)
            (root relativize target).toString
          })
      }
      kind.name -> Value.fromSeq(files.map(Value.of))
    }
    Context(files.get _)
  }

  def generateHtml(cfg: Config, opts: Opts, context: Context): File = {
    val dir = opts.out.filter(_.getExtension != Some("zip")) getOrElse (cfg.getFile("chee.tmpdir") / "gallery")
    dir.createDirectories()
    val template = opts.template.getOrElse {
      mustache.parse(ResourceInfo.galleryTemplate.contentAsString) match {
        case Right(t) => t
        case Left(m) => sys.error(m.toString)
      }
    }
    val target = dir / "index.html"
    if (target.exists) {
      target.delete()
    }
    yamusca.expand.renderTo(template)(context, s => target append s)
    target
  }

  def scaleAction(cfg: Config, opts: Opts): MapGet[Boolean] = {
    val thumb = Thumb.thumbAction(cfg, ProcOpts(), opts.size)
    val scale =
      if (opts.noScale) modify(_.addVirtual(VirtualProperty.defaults.alias(scaledImage -> Ident.path))).flatMap(_ => path.map(Some(_)))
      else Scale.scaleAction(cfg, ProcOpts(), opts.factor, opts.maxLen)
    pair(thumb, scale).flatMap {
      case (ot, os) =>
        val tp: Option[Property] = ot.map(f => Property(thumbnail, f.pathAsString))
        val sp: Option[Property] = os.map(f => Property(scaledImage, f.pathAsString))
        modify { m => m.add(tp).add(sp) } map (_ => os.isDefined)
    }
  }

  def copyAction(id: Ident, subdir: String, out: File, unique: Boolean): MapGet[Boolean] = {
    val dir = out / subdir
    dir.createDirectories()
    valueForce(id).map(File(_)).flatMap { file =>
      val target =
        if (unique) (dir / file.name).makeNonExisting().getOrElse {
          userError(s"Too many equally named files ${(dir / file.name).path}. Rename limit exceeded")
        } else (dir / file.name)
      if (!target.exists) {
        file.copyTo(target)
      }
      add(id -> (out relativize target).toString).map(_ => true)
    }
  }

  def zipAction(id: Ident, subdir: String, out: ZipArchive, unique: Boolean,  entries: AtomicReference[Set[String]]): MapGet[Boolean] = {
    @annotation.tailrec
    def addToZip(f: File, n: Int = 0, max: Int = 9000): String = {
      if (n >= max) sys.error(s"Unable to add ${f.path} to zip file")
      val current = entries.get
      val entryName =
        if (unique) ZipArchive.uniqueEntry(subdir +"/"+ f.name, current)
        else subdir +"/"+ f.name
      if (!current(entryName)) {
        val next = current + entryName
        if (entries.compareAndSet(current, next)) {
          out + (f -> entryName)
          entryName
        } else {
          println("try again…")
          addToZip(f, n + 1, max)
        }
      } else {
        entryName
      }
    }
    valueForce(id).map(File(_)).flatMap { file =>
      val target = addToZip(file)
      add(id -> target).map(_ => true)
    }
  }

  def makeContext: MapGet[Value] =
    get.map { m => MapValue(new LazyMapContext(m), false) }

  case class LazyMapContext(lm: LazyMap) extends Context with LazyLogging {
    def find(key: String): (Context, Option[Value]) = {
      key match {
        case "_format" =>
          def format(pattern: String): ContextFind[String] =
            FormatPatterns.parse(pattern) match {
              case Right(p) =>
                val (next, v) = p.run(lm)
                setHead(LazyMapContext(next)).map(_ =>
                  v.fold({ msg =>
                    logger.warn(s"Error evaluating pattern '$pattern': $msg")
                    ""
                  }, identity))
              case Left(msg) =>
                logger.warn(s"Invalid format pattern '$pattern': $msg")
                ContextFind.unit("")
            }

          (this, Some(Value.lambda(s => format(s.inner.foldLeft("")(_ + _.asString)))))
        case _ =>
          if (!Ident.validate(key)) (this, None)
          else {
            val (next, v) = lm(key)
            (new LazyMapContext(next), v.map(_.value).map(Value.of))
          }
      }
    }
  }

  def setHead(c: Context): ContextFind[Unit] =
    ContextFind.modify { ctx => c :: ctx.tail }

  implicit final class ElementAsString(val el: yamusca.data.Element) extends AnyVal {
    def asString = el match {
      case v: Literal => v.asString
      case v: Variable => v.asString
      case v: Section => v.asString
    }
  }
}
