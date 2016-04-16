package chee.sbt

import sbt._
import sbt.Keys._
import scala.sys.process.Process
import scala.util.{Try, Success, Failure}
import java.time.Instant
import java.util.{Map => JMap, HashMap => JHashMap}

import org.asciidoctor.Asciidoctor
import org.asciidoctor.SafeMode

/** Create html files of all doc files and put them together with the
  * sources in the resources of the project.
  *
  * a list of all commands is generated for the cli help system (cli
  * help system uses adoc files as is). the list is appended to the
  * help command text.
  *
  * see https://github.com/asciidoctor/asciidoctorj
  * see https://github.com/ktoso/asciidoctor-sbt-plugin/
  *
  * Creates a source file containing some information from sbt.
  */
object DocPlugin extends AutoPlugin {
  type Attr = Map[String, AnyRef]

  override def trigger = allRequirements

  object autoImport {
    val CheeDoc = config("CheeDoc") extend Compile
    val docSources = settingKey[File]("The directory containing documentation in asciidoc.")
    val stylesheet = settingKey[String]("The stylesheet to refer to.")
    val cheeDocOpts = settingKey[Seq[String]]("Options for chee when running chee to get usage info")
    val attributes = settingKey[Attr]("The attributes used for documentation generation")
    val options = settingKey[Attr]("The options to pass to the asciidoc engine. This includes the attribute map.")

    val genUsageInfo = taskKey[Map[String, String]]("Generate usage information for all commands.")
    val listDocResources = taskKey[Seq[File]]("Return documentation resources.")
    val genDocResources = taskKey[Seq[File]]("Generate the documentation resources.")
    val genDocInfo = taskKey[Seq[File]]("Generate a scala source file containing build information.")
  }

  import autoImport._

  lazy val docSettings = Seq(
    docSources in CheeDoc := (baseDirectory in Compile).value / "doc",
    stylesheet in CheeDoc := "golo.css",

    attributes in CheeDoc := Map(
      "commit" -> findCurrentCommit((baseDirectory in Compile).value),
      "version" -> version.value,
      "projectName" -> name.value,
      "buildtime" -> buildTime,
      "icons" -> "font",
      "data-uri" -> "true",
      "stylesheet" -> (stylesheet in CheeDoc).value,
      "source-highlighter" -> "prettify"
    ),
    options in CheeDoc := Map(
      "attributes" -> (attributes in CheeDoc).value,
      "safe" -> safeMode //enables the include macro
    ),
    cheeDocOpts := Seq(
      "-Dchee.config=none",
      "-Dchee.workingdir=target",
      "-Dchee.configdir=target",
      "-Dchee.logLevel=OFF"
    ),
    listDocResources in CheeDoc := findDocResources(
      (docSources in CheeDoc).value,
      resourceManaged.value / "chee" / "doc"
    ),
    genDocResources in CheeDoc := generateDocResources(
      streams.value.log,
      (docSources in CheeDoc).value,
      resourceManaged.value / "chee" / "doc",
      {
        val opts = (options in CheeDoc).value
        val cfgFile = readConfigFile((sourceDirectory in Compile).value)
        opts + withAttr(opts)((genUsageInfo in CheeDoc).value ++ Map("chee-configfile" -> cfgFile))
      }
    ),
    genDocResources in CheeDoc <<= (genDocResources in CheeDoc) dependsOn (compile in Compile),
    genDocInfo in CheeDoc := generateDocInfo(
      streams.value.log,
      sourceManaged.value,
      (docSources in CheeDoc).value,
      (attributes in CheeDoc).value
    ),
    genUsageInfo in CheeDoc := generateUsageInfo(
      streams.value.log,
      cheeDocOpts.value,
      "java", //todo find a key that provides this
      (fullClasspath in Runtime).value
    )
  )

  override lazy val projectConfigurations = Seq(CheeDoc)

  override lazy val projectSettings = inConfig(Compile)(docSettings)

  private lazy val engine = {
    val oldTccl = Thread.currentThread().getContextClassLoader()
    try {
      Thread.currentThread().setContextClassLoader(getClass().getClassLoader())
      org.asciidoctor.Asciidoctor.Factory.create()
    } finally {
      Thread.currentThread().setContextClassLoader(oldTccl)
    }
  }

  def generateDocInfo(log: Logger, dir: File, docDir: File, attr: Attr): Seq[File] = {
    import NameFilter._
    log.info("Generate CheeDocInfo file ...")
    val code = s"""package chee.doc
      |object CheeDocInfo {
      |  val version = "{version}"
      |  val buildTime = "{buildtime}"
      |  val commit = "{commit}"
      |  val projectName = "{projectName}"
      |  val docFiles = List(${docDir.listFiles.map(f => f.getName).mkString("\"", "\",\"", "\"")})
      |}""".stripMargin
    val target = dir / "chee" / "doc" / "CheeDocInfo.scala"
    IO.write(target, replaceAttr(code, attr))
    Seq(target)
  }

  def findDocResources(in: File, out: File): Seq[File] = {
    import NameFilter._
    val files: Seq[Seq[File]] = for (file <- IO.listFiles(in, (n: String) => n.matches("^[^_].*?.adoc$"))) yield {
      val outAdoc = out / "adoc" / file.getName
      val outHtml = out / "html" / (IO.split(file.getName)._1 + ".html")
      Seq(outAdoc, outHtml)
    }
    files.flatten
  }

  def generateDocResources(log: Logger, in: File, out: File, options: Attr): Seq[File] = {
    log.info(s"Generate documentation from files in $in ...")
    import NameFilter._

    IO.createDirectories(Seq(out / "adoc", out / "html"))
    val files: Seq[Seq[File]] = for (file <- IO.listFiles(in, (n: String) => n.matches("^[^_].*?.adoc$"))) yield {
      val outAdoc = out / "adoc" / file.getName
      val outHtml = out / "html" / (IO.split(file.getName)._1 + ".html")
      val content = replaceAttr(IO.read(file).replaceAll("""\[subs="[a-zA-Z,]+"\]\n""", ""), options("attributes").asInstanceOf[Attr])
      IO.write(outAdoc, content)

      val opts = options + ("to_file" -> outHtml.toString) + withAttr(options) {
        if (file.getName == "manual.adoc") Map("toc" -> "true", "toc-placement" -> "top")
        else Map.empty[String, AnyRef]
      }
      engine.convertFile(file, asMutable(opts))
      Seq(outAdoc, outHtml)
    }
    files.flatten
  }

  def generateUsageInfo(log: Logger, cheeOpts: Seq[String], javaBin: String, cp: Classpath): Map[String, String] = {
    val commands = List(
      Seq("help"),
      Seq("find"),
      Seq("view"),
      Seq("mktree"),
      Seq("config"),
      Seq("thumb"),
      Seq("scale"),
      Seq("encrypt"),
      Seq("decrypt"),
      Seq("clean"),
      Seq("collection", "edit"),
      Seq("collection", "show"),
      Seq("location", "info"),
      Seq("location", "add"),
      Seq("location", "update"),
      Seq("location", "delete"),
      Seq("location", "mv"),
      Seq("location", "import"),
      Seq("location", "sync"))
    val usage = for (cmd <- commands) yield {
      log.info(s"""Get usage information for command `${cmd.mkString(" ")}'""")
      val opts = Seq("-cp", Attributed.data(cp).mkString(":")) ++ cheeOpts ++ Seq("chee.cli.Main") ++ cmd ++ Seq("--usage")
      s"""usage-${cmd.mkString("-")}""" -> (Process(javaBin, opts).!!).trim
    }
    usage.toMap
  }

  def readConfigFile(src: File): String =
    IO.read(src / "resources" / "application.conf")

  //TODO would be nice if there is a adoc->adoc backend?
  def replaceAttr(content: String, attr: Attr): String = {
    attr.foldLeft(content) { case (str, (k, v)) =>
        str.replace(s"{$k}", v.toString)
    }
  }

  def currentCommitViaGit: Try[String] = Try {
    val hash = (Process("git", Seq("rev-parse", "HEAD")).!!).substring(0, 9)
    Process("git", Seq("name-rev", "HEAD")).!!.split("\\s+", 2).toList match {
      case _ :: name :: Nil => s"${name.trim}/$hash"
      case _ => hash
    }
  }

  def currentCommitViaFiles(basedir: File): Try[String] = Try {
    val List(_, refname) = IO.read(basedir / ".git" / "HEAD").split("\\s+", 2).toList
    val List(_, _, name) = refname.trim.split("/", 3).toList
    val hash = IO.read(basedir / ".git" / refname.trim).trim
    s"$name/${hash.substring(0, 9)}"
  }

  def findCurrentCommit(basedir: File): String = {
    val commit = currentCommitViaGit orElse currentCommitViaFiles(basedir)
    commit match {
      case Failure(ex) =>
        ex.printStackTrace()
        "[unknown commit]"
      case Success(info) => info
    }
  }

  def buildTime: Instant = Instant.now()

  // asciidoctor wants a java.util.HasMap
  def asMutable(m: Attr): JMap[String, AnyRef] = {
    val result: JHashMap[String, AnyRef] = new JHashMap()
    for ((k, v) <- m) {
      v match {
        case cm: Map[_, _] =>
          result.put(k, asMutable(cm.asInstanceOf[Attr]))
        case _ =>
          result.put(k, v)
      }
    }
    result
  }

  def withAttr(map: Attr)(more: Attr): (String, Attr) = {
    val attr = map("attributes").asInstanceOf[Attr]
    ("attributes" -> (attr ++ more))
  }

  val safeMode: java.lang.Integer = SafeMode.UNSAFE.getLevel
}
