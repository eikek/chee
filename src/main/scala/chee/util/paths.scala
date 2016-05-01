package chee.util

import java.nio.file.Path

import better.files._
import chee.properties.MapGet
import chee.properties.MapGet._

/** The db stores relative paths when in repository mode. Client code
  * always sees absolute paths. The following deals with converting
  * them. */
object paths {
  /** Make {{path}} and {{location}} relative to the given directory. */
  def relativeTo(root: File): MapGet[Unit] =
    changePath(relativeStr(root))

  /** Make {{path}} and {{location}} absolute by resolving against the
    * given root directory. */
  def resolveTo(root: File): MapGet[Unit] =
    MapGet.changePath(resolve(root))

  def resolve(root: File)(name: String): String =
    if (java.nio.file.Paths.get(name).isAbsolute) name
    else (root / name).pathAsString

  def relative(root: File)(f: File): Path =
    if (f.path startsWith root.path) root relativize f
    else f.path

  def relativeStr(root: File)(name: String): String =
    if (!name.startsWith(root.pathAsString)) name
    else relative(root)(File(name)).toString
}
