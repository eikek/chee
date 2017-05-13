package chee.cli

import better.files._
import chee.it.CommandSetup.Setup
import chee.TestInfo
import chee.it._
import chee.util.files._
import com.typesafe.scalalogging.LazyLogging
import java.util.zip.ZipFile
import org.scalatest._
import scala.collection.JavaConverters._

class GalleryTest extends FlatSpec with Matchers with CommandSetup with FindHelper with LazyLogging {

  def gallery = new Gallery with BufferOut
  def add  = new Add with BufferOut

  def metaAttach = new MetaAttach with BufferOut

  val copyImagesToDir: Setup => Setup = withSetup { setup =>
    val target = setup.files / "again"
    target.createDirectories()
    setup.files.list.toList.foreach {
      case RegularFile(f) =>
        f.copyTo(target / f.name)
      case _ =>
    }
  }

  val copyImagesToName: Setup => Setup = withSetup { setup =>
    setup.files.list.toList.foreach {
      case RegularFile(f) =>
        f.copyTo(f.mapBaseName(_ + "_1"))
      case _ =>
    }
  }

  val syncLocation: Setup => Setup = withSetup { setup =>
    add.run(setup, "-r", setup.files.pathAsString)
    val (out, Nil) = findLisp(setup)
    out.size should be (TestInfo.images.size * 2)
  }

  val duplicateImagesDir = addImages andThen copyImagesToDir andThen syncLocation
  val duplicateImagesName = addImages andThen copyImagesToName andThen syncLocation

  def testGallery(thumbSize: Int, imageSize: Int, originalSize: Int, setup: Setup): Unit = {
    val cwd = setup.dirs.userDir.get

    val dir = cwd / "gallery"
    gallery.run(setup, "--out", dir.pathAsString, "--link-original")
    (dir / "thumbnails").list should have size (thumbSize.toLong)
    (dir / "originals").list should have size (originalSize.toLong)
    (dir / "images").list should have size (imageSize.toLong)
    (dir / "index.html").exists should be (true)

    val zip = cwd / "gallery.zip"
    gallery.run(setup, "--out", zip.pathAsString, "--link-original")
    zip.exists should be (true)
    val entries = zipEntries(zip)
    entries.filter(_ startsWith "thumbnails") should have size (thumbSize.toLong)
    entries.filter(_ startsWith "originals") should have size (originalSize.toLong)
    entries.filter(_ startsWith "images") should have size (imageSize.toLong)
    entries.filter(_ == "index.html") should have size (1)
  }

  "Gallery" should "not add same thumbnails (1)" in bothChee(duplicateImagesDir) { setup =>
    testGallery(4, 4, 4, setup)
  }

  it should "not add same thumbnails (2)" in bothChee(duplicateImagesName) { setup =>
    testGallery(4, 7, 8, setup)
    // 7 is because only one image needs to be scaled (and is reused)
    // images/nixos_logo_1.png, images/CIMG2590_s.JPG, images/IMG_7437_s.JPG,
    // images/b6bdc5b62c489ebfa55738fb4-scale-1400x933.jpg, images/IMG_7437_s_1.JPG, images/CIMG2590_s_1.JPG,
    // images/nixos_logo.png
  }

  it should "render format patterns" in bothChee(addImages) { setup =>
    val cwd = setup.dirs.userDir.get
    val template = cwd / "template.mustache"
    template.write("{{#files}}{{#_format}}~:length{{/_format}} {{/files}}")
    gallery.run(setup, "--template", template.pathAsString, "--out", (cwd / "gallery").pathAsString)
    (cwd / "gallery" / "index.html").contentAsString.trim should be ("6.6kb 47.6kb 19.4kb 303.8kb")
  }

  it should "render tags and comments" in bothChee(addImages) { setup =>
    // tif files cannot be processed atm
    metaAttach.run(setup, "--skip", "1", "--first", "1", "--tags", "d90a7879", "--comment", "323e-4c15-a1bd-9357121ca8b8", "!ext:tif")
    val (out, Nil) = findLisp(setup, "tag:d90a7879")
    out should have size (1)
    val outfile = setup.dirs.userDir.get / "gallery"
    gallery.run(setup, "--out", outfile.pathAsString, "!ext:tif")
    val html = outfile / "index.html"
    html.exists should be (true)
    val content = html.contentAsString
    val count = countWord(content)_
    count("d90a7879") should be (1)
    count("323e-4c15-a1bd-9357121ca8b8") should be (1)
  }

  def countWord(s: String)(word: String): Int = {
    @annotation.tailrec
    def loop(index: Int = 0, count: Int = 0): Int =
      s.indexOf(word, index) match {
        case n if n < 0 => count
        case n => loop(n + 1, count + 1)
      }
    loop()
  }

  def zipEntries(zip: File): Seq[String] = {
    val zipFile = new ZipFile(zip.toJava)
    val r = zipFile.entries().asScala.map(e => e.getName).toList
    zipFile.close()
    r
  }
}
