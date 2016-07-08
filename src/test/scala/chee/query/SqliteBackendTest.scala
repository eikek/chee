package chee.query

import better.files.File
import chee.metadata.MetadataFile
import org.scalatest._
import chee.properties._
import chee.properties.Patterns._
import chee.TestInfo
import FormatPatterns.lisp
import scala.util.Success
import Index.{LocationInfo, UpdateParam}

class SqliteBackendTest extends FlatSpec with Matchers with chee.FileLoan {

  val filename = Patterns.lookup(Ident.filename).right.result _
  val checksum = Patterns.lookup(Ident.checksum).right.result _

  "find" should "load pages of results" in {
    val sqlite = new SqliteBackend(TestInfo.sampleDb, None, 2)
    val stream = sqlite.find(TrueCondition).get
    stream.map(filename).toList.sorted should be (
      List("CIMG2590_s.JPG", "CIMG2590_s.JPG", "IMG_7437_s.JPG", "IMG_7437_s.JPG", "test1.jpg", "test1.jpg"))
  }

  it should "resolve pixel property" in {
    val sqlite = new SqliteBackend(TestInfo.sampleDb, None, 2)
    val stream = sqlite.find(Prop(Comp.Gt, VirtualProperty.defaults.pixel.ident -> "300")).get
    stream.map(filename).toList.sorted should be (
      List("CIMG2590_s.JPG", "CIMG2590_s.JPG", "IMG_7437_s.JPG", "IMG_7437_s.JPG", "test1.jpg", "test1.jpg"))
  }

  it should "do like on dates" in {
    val sqlite = new SqliteBackend(TestInfo.sampleDb, None, 2)
    val stream = sqlite.find(Prop(Comp.Like, Ident.lastModified -> "2015*")).get
    stream.map(filename).toList.sorted should be (
      List("CIMG2590_s.JPG", "CIMG2590_s.JPG", "IMG_7437_s.JPG", "IMG_7437_s.JPG", "test1.jpg", "test1.jpg"))
  }

  it should "add all properties" in withNewFile { file =>
    val sqlite = new SqliteBackend(file, None)
    val map0 = LazyMap.fromFile(TestInfo.images.head, MetadataFile.empty) + (Ident.location -> "./")
    sqlite.insert(Seq(map0), 0, Progress.empty[Boolean, Int]).get

    val maps = sqlite.find(TrueCondition).get.toList
    maps should have size (1)
    lisp.result(maps(0)) should be (lisp.result(map0))
  }

  it should "use in operator" in {
    val sqlite = new SqliteBackend(TestInfo.sampleDb, None)
    val Success(result1) = sqlite.find(Condition.or(Prop(Comp.Like, Ident.extension -> "jpg"), Prop(Comp.Like, Ident.extension -> "png")))
    val Success(result2) = sqlite.find(In(Ident.extension, List("jpg", "png")))
    result1 should not be ('empty)
    result1.map(lisp.result) should be (result2.map(lisp.result))
  }

  it should "relativize path when in repo mode" in {
    val path = "/home/eike/workspace/projects/chee2/src/test/resources/images/test1.jpg"
    val sqlite = new SqliteBackend(TestInfo.sampleRepoDb, Some(File("/home/eike/workspace")))
    val Success(result1) = sqlite.find(Prop(Comp.Like, Ident.path -> path))
    result1.size should be (1)
    val Success(result2) = sqlite.find(Prop(Comp.Like, Ident.location -> "/home/eike/workspace/projects/chee2"))
    result2.size should be (6)
    val Success(result3) = sqlite.find(In(Ident.path, List(path)))
    result3.size should be (1)
  }

  "exists" should "check for existing files" in withNewFile { file =>
    val sqlite = new SqliteBackend(file, None)
    val map0 = LazyMap.fromFile(TestInfo.images.head)

    sqlite.insert(Seq(map0), 0, Progress.empty[Boolean, Int]).get

    sqlite.idExists.result(LazyMap(Ident.checksum -> "abc")).get should be (false)
    sqlite.idExists.result(map0).get should be (true)
  }

  "insert" should "use real-path if exists" in withNewIndex { index =>
    val data = LazyMap.fromFile(TestInfo.images(1)) + (Index.realPathIdent -> TestInfo.images(0).pathAsString)
    index.insertOne.result(data).get should be (true)
    index.find(Prop(Comp.Eq, Ident.path -> TestInfo.images(1).pathAsString)).get should be ('empty)
    index.find(Prop(Comp.Eq, Ident.path -> TestInfo.images(0).pathAsString)).get should have size (1)
  }

  it should "use path if real-path doesn't exist" in withNewIndex { index =>
    val data = LazyMap.fromFile(TestInfo.images(1))
    index.insertOne.result(data).get should be (true)
    index.find(Prop(Comp.Eq, Ident.path -> TestInfo.images(0).pathAsString)).get should be ('empty)
    index.find(Prop(Comp.Eq, Ident.path -> TestInfo.images(1).pathAsString)).get should have size (1)
  }

  it should "relativise real-path in repo mode" in repoIndex { index =>
    val data = LazyMap(Ident.filename -> "test.jpg", Ident.path -> "", Index.realPathIdent -> "/home/eike/workspace/folder/test.jpg")
    index.insertOne.result(data).get should be (true)
    index.find(Prop(Comp.Eq, Ident.path -> "folder/test.jpg")).get should have size (1)
    index.find(Prop(Comp.Eq, Ident.path -> "space/folder/test.jpg")).get should be ('empty)
  }

  "listLocations" should "list all paths" in {
    val index = new SqliteBackend(TestInfo.sampleDb, None)
    index.listLocations.get should be (Seq(File("/home/eike/workspace/projects/chee2")))
  }

  it should "resove relative paths in repo mode" in {
    val index = new SqliteBackend(TestInfo.sampleRepoDb, Some(File("/home/eike/workspace")))
    index.listLocations.get should be (Seq(File("/home/eike/workspace/projects/chee2")))
  }

  "locationInfo" should "list image counts" in {
    val index = new SqliteBackend(TestInfo.sampleDb, None)
    val info = index.locationInfo.get
    info should be (LocationInfo(Map(File("/home/eike/workspace/projects/chee2") -> 6)))
  }

  it should "list image counts with absolute paths in repo mode" in {
    val index = new SqliteBackend(TestInfo.sampleRepoDb, Some(File("/home/eike/workspace")))
    val info = index.locationInfo.get
    info should be (LocationInfo(Map(File("/home/eike/workspace/projects/chee2") -> 6)))
  }

  "updateOne" should "alter condition when in repository mode" in repoIndex { index =>
    val update = index.updateOne(UpdateParam.updatePathWith('mypath))
    val data = LazyMap(
      Ident("mypath") -> "/home/eike/workspace/projects/chee2/src/test/resources/images/test1.jpg",
      Ident.path -> "/home/eike/workspace/projects/chee2/src/test/resources/photos/test1.jpg"
    )
    update.result(data).get should be (true)
  }

  "update" should "alter condition when in repository mode" in repoIndex { index =>
    val update = UpdateParam.updatePathWith('mypath)
    val data = LazyMap(
      Ident("mypath") -> "/home/eike/workspace/projects/chee2/src/test/resources/images/test1.jpg",
      Ident.path -> "/home/eike/workspace/projects/chee2/src/test/resources/photos/test1.jpg"
    )
    val Success(n) = index.update(Seq(data), update, 0, Progress.count[Boolean])
    n should be (1)
  }

}
