package chee

import better.files.File
import java.util.UUID
import org.scalatest._
import chee.crypto.{Algorithm, CheeCrypt}
import chee.metadata.MetadataFile
import chee.properties._
import chee.query.{Index, SqliteBackend}
import chee.util.files._
import FileOps.{CryptSettings, Result}

class FileOpsTest extends FlatSpec with Matchers with FileLoan {

  val pass = "test123".toCharArray
  val crypt = CryptSettings(pass, MapGet.pair(MapGet.path, MapGet.valueForce(Ident.checksum)).map { case (p, cs) =>
    val f = TestInfo.targetDir / "decrypt" / s"""$cs.${p.stripExtension.getExtension.getOrElse("")}"""
    f.parent.createDirectories()
    f
  })

  "isPathIndexed" should "return true for indexed files" in withIndex { index =>
    val data = index.find(TrueCondition).get
    for (m <- data) {
      FileOps.isPathIndexed(index).result(m).get should be (true)
    }
  }

  it should "return false for empty maps" in withIndex { index =>
    FileOps.isPathIndexed(index).result(LazyMap()).get should be (false)
  }

  it should "return false for non-indexed files" in withIndex { index =>
    val data = index.find(TrueCondition).get
    val nonIndexed = MapGet.filter(data, MapGet.changePath(_ + "/bla").map(_ => true))
    for (m <- nonIndexed) {
      FileOps.isPathIndexed(index).result(m).get should be (false)
    }
  }

  "isFileIndexed" should "return true for indexed files" in withIndex { index =>
    val data = index.find(TrueCondition).get
    for (m <- data) {
      FileOps.isFileIndexed(index, MapGet.path).result(m).get should be (true)
    }
  }

  it should "return false for non-indexed files" in withIndex { index =>
    for (image <- TestInfo.images) {
      FileOps.isFileIndexed(index, MapGet.unit(image)).result(LazyMap()).get should be (false)
    }
  }

  "isEncryptedPathIndexed" should "return path for encrypted paths" in {
    for (f <- TestInfo.images) {
      withNewIndex { index =>
        val enc = LazyMap.fromFile(f) + (Ident.path -> (f.pathAsString + "." + CheeCrypt.passwordEncryptExtension))
        index.insertOne.result(enc).get should be (true)
        val expectedName = f.pathAsString +"."+CheeCrypt.passwordEncryptExtension
        FileOps.isEncryptedPathIndexed(index).result(LazyMap.fromFile(f)).get should be (Some(expectedName))
      }
    }
  }

  "isNonEncryptedPathIndexed" should "return path for non-ecrypted paths" in {
    for (image <- TestInfo.images) {
      withNewIndex { index =>
        val data = LazyMap.fromFile(image)
        index.insertOne.result(data).get should be (true)
        val enc = LazyMap.fromFile(image) + (Ident.path -> (image.pathAsString + "." + CheeCrypt.passwordEncryptExtension))
        FileOps.isNonEncryptedPathIndexed(index).result(enc).get should be (Some(image.pathAsString))
      }
    }
  }

  "updatePath" should "update path with given value" in withIndex { index =>
    val data = index.find(TrueCondition).get.head
    val newPath = "/hello/world"
    FileOps.updatePath(index, newPath).result(data).get should be (Result.Updated)
    val list = index.find(Prop(Comp.Eq, Ident.path -> newPath)).get
    list should have size (1)
    FormatPatterns.lisp.result(data + (Ident.path -> newPath)) should be (
      FormatPatterns.lisp.result(list.head))
  }

  "preprocess" should "add real-path property to processed file" in combine(withExistingFile, withExistingFile) { (f1, f2) =>
    val filter: MapGet[Boolean] = MapGet.add(Ident.path -> f2.pathAsString).map(_ => true)
    val data = LazyMap.fromFile(f1)
    val processed = FileOps.preprocess(filter, MetadataFile.empty).toMap.result(data)
    val expect = LazyMap.fromFile(f2) +
      (Ident.filename -> f1.name) +
      (Index.realPathIdent -> f1.pathAsString) +
      (Ident.lastModified -> MapGet.valueForce(Ident.lastModified).result(data))
    mapsEqual(processed, expect)
  }

  "addPlain" should "add `added` timestamp" in combine(withNewIndex, randomImage) { (index, img) =>
    val added = DateTime.now
    val data = LazyMap.fromFile(img)
    FileOps.addPlain(index, added).result(data).get should be (Result.Added)
    val ts = MapGet.valueForce(Ident.added).result(index.find(TrueCondition).get.head)
    ts should be (added.instant.toString)
  }

  it should "skip existing files" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addPlain(index, DateTime.now).result(data).get should be (Result.Added)
    FileOps.addPlain(index, DateTime.now).result(data).get should be (Result.Skipped)
  }


  def checkEncryptedAdd(index: Index, img: File, enc: File, added: DateTime): Unit = {
    val maps = index.find(Prop(Comp.Eq, Ident.path -> enc.pathAsString)).get
    maps should have size (1)
    val expect = LazyMap.fromFile(img) +
    Extraction.added(added) +
    (Ident.lastModified -> enc.lastModifiedTime.toEpochMilli.toString) +
    (Ident.path -> enc.pathAsString) +
    (Ident.filename -> enc.name.substring(0, enc.name.length -4))

    mapsEqual(maps.head, expect) should be (true)
  }

  "addEncrypted" should "add properties of decrypted file but path to encrypted file" in {
    combine(withNewIndex, randomImage) { (index, img) =>
      encryptFile(img, pass) { enc =>
        val added = DateTime.now
        val data = LazyMap.fromFile(enc)
        FileOps.addEncrypted(index, added, Some(crypt)).result(data).get should be (Result.Added)
        checkEncryptedAdd(index, img, enc, added)
      }
    }
  }

  it should "skip existing files" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val added = DateTime.now
      val data = LazyMap.fromFile(enc)
      FileOps.addEncrypted(index, added, Some(crypt)).result(data).get should be (Result.Added)
      FileOps.addEncrypted(index, added, Some(crypt)).result(data).get should be (Result.Skipped)
    }
  }

  "addToIndex" should "add non existing plain files" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    val now = DateTime.now
    FileOps.addToIndex(index, now, None).result(data).get should be (Result.Added)
    FileOps.addToIndex(index, now, None).result(data).get should be (Result.Skipped)
  }

  it should "update path if adding encrypted files" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    val now = DateTime.now
    FileOps.addToIndex(index, now, None).result(data).get should be (Result.Added)
    encryptFile(img, pass) { enc =>
      val data2 = LazyMap.fromFile(enc)
      FileOps.addToIndex(index, now, Some(crypt)).result(data2).get should be (Result.Updated)
    }
  }

  it should "add encrypted files" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      val now = DateTime.now
      FileOps.addToIndex(index, now, Some(crypt)).result(data).get should be (Result.Added)
      checkEncryptedAdd(index, img, enc, now)
      FileOps.addToIndex(index, now, Some(crypt)).result(data).get should be (Result.Skipped)
    }
  }

  it should "update path if adding plain files" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      val now = DateTime.now
      FileOps.addToIndex(index, now, Some(crypt)).result(data).get should be (Result.Added)

      val data2 = LazyMap.fromFile(img)
      FileOps.addToIndex(index, now, None).result(data2).get should be (Result.Updated)
    }
  }

  it should "skip crypted files without decrypt settings" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      FileOps.addToIndex(index, DateTime(100), None).result(data).get should be (Result.Skipped)
    }
  }


  "checksumMatch" should "return true for same sums" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get
    FileOps.checksumMatch(index).result(data).get should be (true)
  }

  it should "return false for different sums" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get
    val other = data + (Ident.checksum -> "aabbee")
    FileOps.checksumMatch(index).result(other).get should be (false)
  }

  it should "throw if no checksum" in withNewIndex { index =>
    FileOps.checksumMatch(index).result(LazyMap()) should be ('Failure)
  }

  it should "throw if not in index" in combine(withNewIndex, randomImage) { (index, img) =>
    FileOps.checksumMatch(index).result(LazyMap.fromFile(img)) should be ('Failure)
  }

  "checksumExists" should "return true for existing files" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get
    FileOps.checksumExists(index).result(data).get should be (true)
  }

  it should "return false for non-existing files" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.checksumExists(index).result(data).get should be (false)
  }

  "syncWithIndex" should "skip existing plain files with same checksum" in combine(withNewIndex, randomImage) { (index, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get
    FileOps.syncWithIndex(index, DateTime.now, None).result(data).get should be (Result.Ok)
  }

  it should "skip existing encrypted files with same checksum" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      FileOps.addToIndex(index, DateTime.now, Some(crypt)).result(data).get
      FileOps.syncWithIndex(index, DateTime.now, Some(crypt)).result(data).get should be (Result.Ok)
    }
  }

  it should "update path for same encrypted file" in combine(withNewIndex, randomImage) { (index, img) =>
    FileOps.addToIndex(index, DateTime(100), None).result(LazyMap.fromFile(img)).get

    val db = index.find(TrueCondition).get.head
    encryptFile(img, pass) { enc =>
      img.delete()
      val data = LazyMap.fromFile(enc)
      FileOps.syncWithIndex(index, DateTime.now, Some(crypt)).result(data).get should be (Result.Updated)
      val list = index.find(TrueCondition).get
      list should have size (1)
      mapsEqual(list.head, db + (Ident.path -> enc.pathAsString))
    }
  }

  it should "update path for same plain file" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      FileOps.addToIndex(index, DateTime(100), Some(crypt)).result(LazyMap.fromFile(enc)).get

      val db = index.find(TrueCondition).get.head
      enc.delete()
      val data = LazyMap.fromFile(img)
      FileOps.syncWithIndex(index, DateTime.now, None).result(data).get should be (Result.Updated)
      val list = index.find(TrueCondition).get
      list should have size (1)
      mapsEqual(list.head, db + (Ident.path -> img.pathAsString))
    }
  }

  it should "update all properties if checksum changed (plain)" in combine(withNewIndex, randomImage) { (index, img) =>
    FileOps.addToIndex(index, DateTime(100), None).result(LazyMap.fromFile(img)).get

    img `<<` "some text"
    FileOps.syncWithIndex(index, DateTime(200), None).result(LazyMap.fromFile(img)).get should be (Result.Updated)
    val list = index.find(TrueCondition).get
    list should have size (1)
    withNewIndex { index2 =>
      FileOps.addToIndex(index2, DateTime(100), None).result(LazyMap.fromFile(img)).get
      val other = index2.find(TrueCondition).get.head
      mapsEqual(other, list.head)
    }
  }

  it should "update all properties if checksum changed (encrypted)" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      FileOps.addToIndex(index, DateTime(100), Some(crypt)).result(LazyMap.fromFile(enc)).get
      img `<<` "some text"

      FileOps.syncWithIndex(index, DateTime(200), None).result(LazyMap.fromFile(img)).get should be (Result.Updated)
      val list = index.find(TrueCondition).get
      list should have size (1)
      withNewIndex { index2 =>
        FileOps.addToIndex(index2, DateTime(100), None).result(LazyMap.fromFile(img)).get
        val other = index2.find(TrueCondition).get.head
        mapsEqual(other, list.head)
      }
    }
  }

  it should "add new plain files" in combine(withNewIndex, withNewIndex, randomImage) { (index1, index2, img) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index1, DateTime(100), None).result(data).get
    FileOps.syncWithIndex(index2, DateTime(100), None).result(data).get should be (Result.Added)

    val m1 = index1.find(TrueCondition).get.head
    val m2 = index2.find(TrueCondition).get.head
    mapsEqual(m1, m2)
  }

  it should "add new encrypted files" in combine(withNewIndex, withNewIndex, randomImage) { (index1, index2, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      FileOps.addToIndex(index1, DateTime(100), Some(crypt)).result(data).get
      FileOps.syncWithIndex(index2, DateTime(100), Some(crypt)).result(data).get should be (Result.Added)

      val m1 = index1.find(TrueCondition).get.head
      val m2 = index2.find(TrueCondition).get.head
      mapsEqual(m1, m2)
    }
  }

  it should "skip crypted files without decrypt settings" in combine(withNewIndex, randomImage) { (index, img) =>
    encryptFile(img, pass) { enc =>
      val data = LazyMap.fromFile(enc)
      FileOps.syncWithIndex(index, DateTime(100), None).result(data).get should be (Result.Skipped)
    }
  }

  "copyFile" should "copy file relative to source root" in randomImage { img =>
    val data = LazyMap.fromFile(img)
    newDirectory { targetDir =>
      FileOps.copyFile(targetDir, Some(img.parent.parent)).result(data).get should be (())
      val expect = targetDir / img.parent.name / img.name
      expect.exists should be (true)
      expect.sha256 should be (img.sha256)
    }
  }

  it should "copy file in targetDir when no source root" in randomImage { img =>
    val data = LazyMap.fromFile(img)
    newDirectory { targetDir =>
      FileOps.copyFile(targetDir, None).result(data).get should be (())
      val expect = targetDir / img.name
      expect.exists should be (true)
      expect.sha256 should be (img.sha256)
    }
  }

  it should "rename files until non-existing" in randomImage { img =>
    val data = LazyMap.fromFile(img)
    newDirectory { targetDir =>
      FileOps.copyFile(targetDir, None).result(data).get should be (())
      FileOps.copyFile(targetDir, None).result(data).get should be (())

      val expect = List(targetDir / img.name, targetDir / img.mapBaseName(_+"-1").name)
      expect.foreach { e =>
        e.exists should be (true)
        e.sha256 should be (img.sha256)
      }
    }
  }

  "importFile" should "import non existing files" in combine(withNewIndex, randomImage, newDirectory) { (index, img, target) =>
    val data = LazyMap.fromFile(img)
    FileOps.importFile(index, DateTime.now, None, target).result(data).get should be (Result.Added)
    (target / img.name).exists should be (true)
    index.find(TrueCondition).get should have size (1)
  }

  it should "skip existing files by default" in combine(withNewIndex, randomImage, newDirectory) { (index, img, target) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get

    FileOps.importFile(index, DateTime.now, None, target).result(data).get should be (Result.Duplicate)
    (target / img.name).exists should be (false)
  }

  it should "add existing files when told so" in combine(withNewIndex, randomImage, newDirectory) { (index, img, target) =>
    val data = LazyMap.fromFile(img)
    FileOps.addToIndex(index, DateTime.now, None).result(data).get

    FileOps.importFile(index, DateTime.now, None, target, duplicates = true).result(data).get should be (Result.Added)
    (target / img.name).exists should be (true)
    index.find(TrueCondition).get should have size (2)
  }

  // TODO tests for import encrypted files


  def mapsEqual(m1: LazyMap, m2: LazyMap): Boolean = {
    FormatPatterns.lisp.result(m1) should be (FormatPatterns.lisp.result(m2))
    true
  }

}
