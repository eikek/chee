package chee.cli

import better.files._
import chee.util.files._
import chee.query.Index

object Location {
  /** Test whether `f` is inside a location given by `locations`.
    *
    * Return the location that `f` is a child of, or `None`.
    */
  private def findFileLocation(locations: Set[File])(f: File): Option[File] =
    locations.find(_ parentOf f)

  /** Filter a list of directories by whether they are childs of known
    * locations.
    *
    * Apply the `include` function to the result of `findFileLocation`
    * and if `true` include dir (from `dirs`) in the result.
    */
  private def filterFileLocation(index: Index, include: Option[File] => Boolean)(dirs: Seq[File]): Seq[File] = {
    val existing = index.listLocations().get.toSet
    val check = findFileLocation(existing)_
    dirs.filter(d => include(check(d)))
  }

  private def checkFileLocation(index: Index, msg: String, err: Option[File] => Boolean, dirs: Seq[File]): Unit = {
    val failedDirs = filterFileLocation(index, err)(dirs)
    if (failedDirs.isEmpty) ()
    else userError(failedDirs.map(d => s"`${d.path}' $msg").mkString("\n"))
  }

  /** Check if all `dirs` are known locations (or childs thereof). */
  def checkRegisteredLocations(index: Index, dirs: Seq[File]): Unit =
    checkFileLocation(index, "is not a known location", _.isEmpty, dirs)

  /** Check if all `dirs` are not known locations. */
  def checkNotRegisteredLocations(index: Index, dirs: Seq[File]): Unit =
    checkFileLocation(index, "is a known location", _.nonEmpty, dirs)

}
