package chee.query

import better.files._
import chee.metadata.MetadataFile
import chee.properties._
import Predicates._

object FileBackend {

  /** Create a stream of all files and directories below the given
    * path.
    */
  def walk(start: File, recursive: Boolean): Stream[File] = {
    if (recursive) start.glob("**")(File.PathMatcherSyntax.glob).toStream
    else start.list.toStream
  }

  /** Return a stream of files below the given directory. Directories
    * are skipped.
    */
  def walkFiles(file: File, recursive: Boolean): Stream[File] =
    walk(file, recursive).filterNot(_.isDirectory)

  /** Find files below the given directory and create a property map of
    * each.
    */
  def walkProperties(file: File, recursive: Boolean, mf: MetadataFile): Stream[LazyMap] =
    walkFiles(file, recursive)
      .map(LazyMap.fromFile(_, mf)
        .add(Ident.location -> file.path.toString))

  def find(pred: Predicate, file: File, recursive: Boolean, mf: MetadataFile): Stream[LazyMap] =
    MapGet.filter(walkProperties(file, recursive, mf), pred)

  /** Find all files below the given directory that pass the given
    * Condition. Return a stream of property maps of each file.
    */
  def find(cond: Condition, file: File, recursive: Boolean, mf: MetadataFile): Stream[LazyMap] =
    find(Predicates(cond), file, recursive, mf)

}
