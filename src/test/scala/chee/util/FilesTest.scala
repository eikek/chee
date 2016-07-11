package chee.util

import org.scalatest._
import chee.FileLoan
import files._

class FilesTest extends FlatSpec with Matchers with FileLoan {

  "makeNonExisting" should "append numbers to files without extension" in withExistingFile { f =>
    val f2 = f.makeNonExisting().get
    f2.name should be (f.name + "-1")
  }

  it should "append numbers to basename" in withExistingFile { f =>
    val f2 = f.renameTo(f.name + ".txt")
    val f3 = f2.makeNonExisting().get
    f3.name should be (f.name + "-1.txt")
  }

  it should "count up if number exists" in withExistingFile { f =>
    val f2 = f.makeNonExisting().get.createIfNotExists()
    val f3 = f2.makeNonExisting().get
    f3.name should be (f.name + "-2")
  }

  it should "stop for too many tries" in withExistingFile { f =>
    f.makeNonExisting(1) should be (None)
  }

  "mapBaseName" should "work without extension" in withNewFile { f =>
    val f2 = f.mapBaseName(_ + "-bla")
    f2.name should be (f.name + "-bla")
  }

  it should "work with extension" in withNewFile { f =>
    val f2 = f.sibling(f.name + ".txt")
    val f3 = f2.mapBaseName(_ + "-bla")
    f3.name should be (f.name + "-bla.txt")
  }
}
