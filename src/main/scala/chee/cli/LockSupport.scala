package chee.cli

import com.typesafe.config.Config

trait LockSupport  {

  import better.files._
  import chee.CheeConf.Implicits._

  private def dolock(cfg: Config, name: String): File = {
    val f = cfg.getFile("chee.tmpdir") / name
    try {
      java.nio.file.Files.createFile(f.path)
      f
    } catch {
      case e: java.nio.file.FileAlreadyExistsException =>
        chee.UserError(s"Another instance of chee is already running.\nIf you are sure this is wrong, delete lock file `${f.path}'.")
    }
  }

  private def unlock(f: File) = {
    if (f.exists) f.delete(true)
  }

  def withLock(cfg: Config, name: String = "lock")(body: => Unit): Unit = {
    val lock = dolock(cfg, name)
    try {
      body
    } finally {
      unlock(lock)
    }
  }
}
