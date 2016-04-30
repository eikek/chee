package chee.conf

import better.files._
import com.typesafe.config.{ Config, ConfigFactory }
import chee.util.files._

case class Directories(
  initial: Config = ConfigFactory.empty(),
  userDir: Option[File] = None,
  configDir: Option[File] = None,
  workingDir: Option[File] = None,
  repoRoot: Option[File] = None,
  userConfig: Option[File] = None)

object Directories {

  /** Create an initial config for working inside the given repository
    * root. {{repoRoot}} is the directory containing the {{.chee}}
    * directory. */
  def atRepoRoot(repoRoot: File): Directories = {
    val configDir = repoRoot / ".chee"
    Directories(
      configDir = Some(configDir),
      workingDir = Some(configDir / "work"),
      repoRoot = Some(repoRoot),
      userConfig = (configDir / "chee.conf").existing)
  }

  /** Create an initial config for using chee globally. */
  def global: Directories = {
    Directories(
      userConfig = (File.home / ".chee.conf") ||
                   (File.home / ".config" / "chee" / "chee.conf"))
  }

  /** Create an initial config to store chee data inside the given
    * directory. This is almost like {{atRepoRoot}} but doesn't set
    * the repository root directory. This is intented to be used in
    * tests.*/
  def inDirectory(dir: File): Directories = {
    val configDir = dir / ".chee-dir"
    Directories(
      userDir = Some(dir),
      configDir = Some(configDir),
      workingDir = Some(configDir / "work"),
      userConfig = (configDir / "chee.conf").existing)
  }

  def findDotChee(dir: File): Option[File] = {
    @scala.annotation.tailrec
    def loop(d: File): Option[File] = d / ".chee" match {
      case f if f.isDirectory => Some(f)
      case _ => Option(d.parent) match {
        case Some(f) => loop(f)
        case _ => None
      }
    }
    loop(dir)
  }

  def makeDefault: Directories =
    findDotChee(File.currentWorkingDirectory)
      .map(_.parent)
      .map(atRepoRoot)
      .getOrElse(global)

}
