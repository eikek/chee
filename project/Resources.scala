package chee.sbt

import sbt._
import sbt.Keys._
import java.nio.file.{Files, Paths, StandardCopyOption}

object Resources extends AutoPlugin {

  object autoImport {
    case class Lib(version: String, url: String, kind: Symbol, category: Option[String] = None) {
      lazy val realUrl = sbt.url(url.replace("<version>", version))
      def in(category: String): Lib = copy(category = Some(category))
    }

    object Lib {
      def js(version: String, url: String): Lib = Lib(version, url, 'js)
      def css(version: String, url: String): Lib = Lib(version, url, 'css)
      def font(version: String, url: String): Lib = Lib(version, url, 'fonts)
      def img(version: String, url: String): Lib = Lib(version, url, 'img)
    }

    val resourceLibs = settingKey[Seq[Lib]]("External resource dependencies")
    val fetchResources = taskKey[Seq[File]]("Fetch external resources")
    val resourceInfo = taskKey[File]("Generate a resource info source file")
  }

  import autoImport._

  lazy val resourceSettings = Seq(
    resourceLibs := Seq.empty[Lib],
    resourceInfo := writeResourceInfo.value,
    fetchResources := fetchResourcesImpl.value
  )


  lazy val makeFileName = (version: String, url: URL) => {
    val name = Paths.get(url.getPath).getFileName.toString
    if (name contains version) name
    else version + java.io.File.separator + name
  }

  lazy val fetchResourcesImpl = Def.task {
    val dir = (resourceManaged in Compile).value / "chee" / "resources"
    val files = resourceLibs.value.map {
      case lib@Lib(version, _, kind, category) =>
        val out = dir / (category.getOrElse("all")) / kind.name
        val url = lib.realUrl
        val outFile = out / makeFileName(version, url)
        if (!outFile.exists) {
          streams.value.log.info(s"Downloading $url -> ${outFile.getName} …")
          val conn = url.openConnection()
          conn.connect()
          val inStream = conn.getInputStream
          IO.createDirectories(Seq(outFile.getParentFile))
          Files.copy(inStream, outFile.toPath, StandardCopyOption.REPLACE_EXISTING)
          inStream.close
        }
        outFile
    }
    files.toSeq
  }

  lazy val writeResourceInfo = Def.task {
    streams.value.log.info("Generate ResourceInfo …")
    val libList = resourceLibs.value.map(l => s"""("${l.category.getOrElse("all")}", '${l.kind.name}, "${makeFileName(l.version, l.realUrl)}")""").toList
    val code = s"""package chee.resources
    |import java.net.URL
    |object ResourceInfo {
    |  def urlFor(kind: Symbol, file: String, category: String = "all"): URL = {
    |    val name = category + "/" + kind.name + "/" + file
    |    Option(getClass.getResource(name)).getOrElse(sys.error(s"$$name not found in classpath!"))
    |  }
    |
    |  val libraries: List[(String, Symbol, String)] = ${libList}
    |  val kinds: List[Symbol] = List('js, 'img, 'fonts, 'img)
    |  val categories: Set[String] = libraries.map(_._1).toSet
    |  def get(category: String): Map[Symbol, List[URL]] = {
    |    val zero = Map.empty[Symbol, List[URL]]
    |    libraries.filter(_._1 == category).foldLeft(zero) { case (map, (cat, kind, name)) =>
    |      val urls: List[URL] = map.get(kind).getOrElse(Nil)
    |      map + (kind -> (urls :+ urlFor(kind, name, category)))
    |    }
    |  }
    |  def allOf(category: String): Map[Symbol, List[URL]] = {
    |    val all = get("all")
    |    if (category == "all") all
    |    else {
    |      val other = get(category)
    |      all.map({ case (k, v) => k -> (other.get(k).getOrElse(List.empty[URL]) ::: v) })
    |    }
    |  }
    |  val galleryTemplate = urlFor('html, "gallery-template.mustache")
    |}
    """.stripMargin

    val file = (sourceManaged in Compile).value / "chee" / "resources" / "ResourceInfo.scala"
    IO.write(file, code)
    file
  }

  override def trigger = allRequirements

  override lazy val projectSettings = inConfig(Compile)(resourceSettings)
}
