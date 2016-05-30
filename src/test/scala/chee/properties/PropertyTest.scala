package chee.properties

import chee.metadata.MetadataFile
import scala.util._
import org.scalatest._
import better.files._
import chee.TestInfo
import chee.util.files._

class PropertyTest extends FlatSpec with Matchers with chee.FileLoan {
  val idMapping = (id: Ident) => id.in("old")

  "digest" should "throw for non existing file" in withNewFile { file =>
    val Failure(ex) = Try(ChecksumExtract.digestFile("sha1")(file))
    ex.getClass() should be (classOf[java.nio.file.NoSuchFileException])
  }

  "basicProperties" should "throw on non-existent file" in withNewFile { file =>
    val Failure(ex) = Try(new BasicExtract().basicFile(file))
    ex.getClass() should be (classOf[java.nio.file.NoSuchFileException])
  }

  it should "work for existing files" in {
    val map = new BasicExtract().extract(TestInfo.images.head)
    val expectedIdents = Set(Ident.path, Ident.filename, Ident.extension, Ident.mimetype, Ident.length, Ident.lastModified)
    map.asSet.map(_.ident).toSet should be (expectedIdents)
    map.get(Ident.filename) should be (Some(TestInfo.images.head.name))
    map.get(Ident.extension) should be (TestInfo.images.head.getExtension)
    map.get(Ident.path) should be (Some(TestInfo.images.head.path.toString))
    map.get(Ident.mimetype) should (be (Some("image/jpg")) or be (Some("image/png")) or be (Some("image/jpeg")))
  }

  it should "apply mapIdents" in {
    val extr = new BasicExtract().mapIdents(idMapping)
    extr.idents should be (new BasicExtract().idents.map(idMapping))
    val map = extr.extract(TestInfo.images.head)
    for (id <- extr.idents) map(id) match {
      case Some(_) =>
      case None => fail(s"no value for ${id.name}")
    }
  }

  "checksum" should "fail for non existing files" in withNewFile { f =>
    val Failure(ex) = Try(ChecksumExtract.checksum(f))
    ex.getClass() should be (classOf[java.nio.file.NoSuchFileException])
  }

  it should "work for existing files" in {
    val checksum = new ChecksumExtract().checksum(TestInfo.baseDir / "build.sbt")
    checksum.ident should be (Ident.checksum)
    checksum.value.length should be (64)
  }

  it should "apply mapIdents" in {
    val extr = new ChecksumExtract().mapIdents(idMapping)
    extr.idents should be (new ChecksumExtract().idents.map(idMapping))
    val pm = extr.extract(TestInfo.images.head)
    for (id <- extr.idents) pm(id) match {
      case Some(_) =>
      case None => fail(s"No value for id ${id.name}")
    }
  }

  "imageProperties" should "fail for non existing files" in withNewFile { f =>
    val Failure(ex) = Try(new ImageExtract().extract(f))
    ex.getClass() should be (classOf[java.nio.file.NoSuchFileException])
  }

  it should "work for existing file" in {
    for (file <- TestInfo.images if file.extension != Some(".png")) {
      val props = new ImageExtract().extract(file)
      for (id <- (Ident.imageProperties.toSet - Ident.iso)) {
        props.get(id) should not be (None)
      }
    }
  }

  it should "return empty map for unsupported files" in withExistingFile { f =>
    new ImageExtract().extract(TestInfo.baseDir / "readme.org") should be (PropertyMap.empty)
    new ImageExtract().extract(f) should be (PropertyMap.empty)
  }

  it should "apply mapIdents" in {
    val extr = new ImageExtract().mapIdents(idMapping)
    extr.idents should be (new ImageExtract().idents.map(idMapping))
    val pm = extr.extract(TestInfo.images.filter(_.name.startsWith("IMG")).head)
    for (id <- extr.idents) pm(id) match {
      case Some(_) =>
      case None => fail(s"No value for id ${id.name}")
    }
  }

  "width, height and iso" should "be numeric values" in {
    for (img <- TestInfo.images) {
      val props = new WidthHeightExtract().extractM(img).result(LazyMap(Ident.mimetype -> "image/jpg"))
      props.get(Ident.height).get should fullyMatch regex ("[0-9]+".r)
      props.get(Ident.width).get should fullyMatch regex ("[0-9]+".r)
    }
    for (img <- TestInfo.images) {
      val m = LazyMap.fromFile(img)
      MapGet.valueForce(Ident.height).result(m) should fullyMatch regex ("[0-9]+".r)
      MapGet.valueForce(Ident.width).result(m) should fullyMatch regex ("[0-9]+".r)
    }
  }

  "length, added" should "be numeric timestamp" in {
    for (img <- TestInfo.images) {
      val props = new BasicExtract().basicFile(img) + Extraction.added(DateTime.now)
      props.get(Ident.length).get should fullyMatch regex "[0-9]+"
      props.get(Ident.added).get should fullyMatch regex "[0-9]+"
    }
  }

  "created" should "be a LocalDateTime" in {
    for (img <- TestInfo.images) {
      val props = new ImageExtract().extract(img)
      props.get(Ident.created) match {
        case Some(date) =>
          val dt = LocalDateTimeConverter.parse(date)
          dt.right.get.asString should be (date)
        case _ =>
      }
    }
  }

  "basic and image properties" should "make up default idents without virtual" in {
    (Ident.fileProperties ++ Ident.imageProperties) should be (
      Ident.defaults diff VirtualProperty.idents.all)
  }

  "extension" should "find chars after last dot" in {
    def extension(f: File) = f.getExtension
    extension(file"test.jpg") should be (Some("jpg"))
    extension(file"test.x.jpg") should be (Some("jpg"))
    extension(file"test.a_.jpg.jpg") should be (Some("jpg"))
    extension(file"a.p.l.o.a.i.x") should be (Some("x"))
    extension(file"/home/test/a/b/readme.org") should be (Some("org"))
    extension(file".test") should be (None)
    extension(file"test.") should be (None)
    extension(file"test") should be (None)
  }
}
