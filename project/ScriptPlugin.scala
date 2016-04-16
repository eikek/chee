package chee.sbt

import sbt._
import sbt.Keys._
import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission._
import java.util.EnumSet

object ScriptPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val script = config("script") extend Compile

    val shebang = settingKey[String]("The full path to the shell used as shebang for the script")
    val javaBin = settingKey[String]("The full path to java executable.")
    val assemblyDir = settingKey[String]("The directory where the assembly jar is located. This is a directory relative to the zip root.")
    val `gen-chee` = taskKey[File]("Create the assembly and shell script")
    val `gen-zip` = taskKey[File]("Create a zip file containing chee.")
  }

  import autoImport._

  lazy val scriptSettings = Seq(
    shebang in script := "/bin/sh",
    assemblyDir in script := "./",
    javaBin in script := "java",
    javaOptions in script := Seq(),
    `gen-chee` in Compile := genBinImpl.value,
    `gen-zip` in Compile := genZipImpl.value
  )

  def makeScript(sourceDir: File, classpath: String, shebang: String, java: String, javaOpts: String, cdtohere: Boolean = false): String = {
    val template = Template(sourceDir / "shell" / "chee")
    template.render(Map(
      "cdtohere" -> cdtohere,
      "shebang" -> shebang,
      "options" -> javaOpts,
      "javabin" -> java,
      "classpath" -> classpath
    ))
  }

  def setExecutable(f: File): File = {
    Files.setPosixFilePermissions(f.toPath, EnumSet.of(
      OWNER_READ, OWNER_WRITE, OWNER_EXECUTE,
      GROUP_READ, GROUP_EXECUTE,
      OTHERS_READ, OTHERS_EXECUTE
    ))
    f
  }

  lazy val genBinImpl = Def.task {
    val template = sourceDirectory.value / "shell" / "chee"
    val out = target.value / "bin" / "chee"
    val body = makeScript(
      sourceDirectory.value,
      Attributed.data((fullClasspath in Runtime).value).mkString(java.io.File.pathSeparator),
      (shebang in script).value,
      (javaBin in script).value,
      (javaOptions in script).value.mkString(" "))
    IO.write(out, body)
    setExecutable(out)
    out
  }

  lazy val genZipImpl = Def.task {
    streams.value.log.info("Creating zip file.")
    val zipDir = target.value / (name.value + "-" + version.value)
    val zipFile = target.value / (name.value +"-"+ version.value +".zip")
    val libs = for {
      lib <- (Attributed.data((fullClasspath in Runtime).value) :+ (packageBin in Compile).value)
      if lib.isFile
    } yield {
      IO.copyFile(lib, zipDir / "lib" / lib.getName)
      "lib" + java.io.File.separator + lib.getName
    }
    val body = makeScript(
      sourceDirectory.value,
      libs.mkString(java.io.File.pathSeparator),
      (shebang in script).value,
      (javaBin in script).value,
      (javaOptions in script).value.mkString(" "),
      true)

    IO.write(zipDir / "chee", body)
    setExecutable(zipDir / "chee")
    IO.zip(zipEntries(zipDir), zipFile)
    zipFile
  }

  def zipEntries(dir: File): Seq[(File, String)] = {
    @scala.annotation.tailrec
    def loop(dirs: List[File], result: List[(File, String)] = Nil): List[(File, String)] =
      dirs match {
        case Nil => result
        case f :: ds if f.isFile =>
          IO.relativize(dir, f) match {
            case Some(name) => loop(ds, (f -> name) :: result)
            case _ => sys.error(s"$f is not parent of $dir")
          }
        case d :: ds if d.isDirectory =>
          loop(ds ::: IO.listFiles(d).toList, result)
        case x :: _ =>
          sys.error(s"Cannot add zip entry for $x")
      }
    loop(IO.listFiles(dir).toList)
  }

  override lazy val projectSettings = inConfig(Compile)(scriptSettings)

}
