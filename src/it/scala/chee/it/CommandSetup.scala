package chee.it

import better.files._
import chee.conf._
import chee.cli.{ Command, LocationAdd }
import chee.query.SqliteBackend
import com.typesafe.config.{ Config, ConfigFactory, ConfigValueFactory }
import java.util.UUID
import scala.util.{ Try, Success, Failure }
import chee.TestInfo
import CommandSetup._

trait CommandSetup {

  private def mkDir: File = {
    val name = UUID.randomUUID().toString
    val dir = TestInfo.targetDir / "it" / name
    dir.createDirectories()
    dir
  }

  private val globalSetup: File => Directories =
    dir => Directories.inDirectory(dir)

  private val repoSetup: File => Directories = { dir =>
    val dirs = Directories.atRepoRoot(dir).copy(userDir = Some(dir))
    dirs.configDir.foreach(_.createDirectories())
    dirs
  }

  private val addImages: Setup => Setup = { setupVals =>
    val pics = setupVals.files
    pics.createIfNotExists(asDirectory = true)
    TestInfo.images.foreach(f => f.copyTo(pics / f.name))
    val addCmd = new LocationAdd with BufferOut
    val addOpts = LocationAdd.Opts(dirs = Seq(pics), recursive = true)
    val sqlite = new SqliteBackend(setupVals.cfg.getIndexDb)
    addCmd.indexDirs(setupVals.cfg, addOpts, sqlite)
    setupVals
  }

  private def teardown(setupVals: Setup) = {
    setupVals.userDir.delete()
  }

  def cheeSetup(before: File => Setup)(code: Setup => Any): Unit = {
    val setupVals = before(mkDir)
    // assume the tmpdir exists; this is ensured in Main
    setupVals.cfg.getFile("chee.tmpdir").createDirectories()
    val test = Try(code(setupVals))
    teardown(setupVals)
    test match {
      case Failure(ex) => throw ex
      case _ =>
    }
  }

  def globalChee(code: Setup => Any): Unit = {
    cheeSetup(globalSetup andThen Setup.apply)(code)
  }

  def globalCheeWithImages(code: Setup => Any): Unit =
    cheeSetup(globalSetup andThen Setup.apply andThen addImages)(code)

  def repoRoot(code: Setup => Any): Unit =
    cheeSetup(repoSetup andThen Setup.apply)(code)

  def repoRootWithImages(code: Setup => Any): Unit =
    cheeSetup(repoSetup andThen Setup.apply andThen addImages)(code)

  implicit class CommandBufferRun(cmd: Command with BufferOut) {
    def run(setup: Setup, args: String*): (List[String], List[String]) = {
      cmd.exec(setup.cfg, args.toArray)
      (cmd.stdoutLines, cmd.stderrLines)
    }
  }
}

object CommandSetup {
  case class Setup(dirs: Directories) {
    val cfg = CheeConf.load(dirs)
    val userDir = dirs.userDir match {
      case Some(dir) => dir
      case _ => sys.error("Invalid test configuration. No user dir set.")
    }
    val files = userDir / "files"
  }
}
