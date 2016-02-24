package chee.sbt

import sbt._
import sbt.Keys._
import sbtassembly.AssemblyPlugin.autoImport._
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

  def makeScript(sourceDir: File, jar: String, shebang: String, java: String, javaOpts: String): String = {
    val template = sourceDir / "shell" / "chee"
    IO.read(template)
      .replace("$assembly-jar$", jar)
      .replace("$shebang$", shebang)
      .replace("$java-bin$", java)
      .replace("$options$", javaOpts)
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
      assembly.value.toString,
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
    val assDir = (assemblyDir in script).value
    val body = makeScript(
      sourceDirectory.value,
      s"$$(dirname $$0)/${assDir}/${assembly.value.getName}",
      (shebang in script).value,
      (javaBin in script).value,
      (javaOptions in script).value.mkString(" "))

    IO.write(zipDir / "chee", body)
    setExecutable(zipDir / "chee")
    IO.copyFile(assembly.value, zipDir / assDir / assembly.value.getName)
    IO.zip(zipDir.listFiles.map(f => f -> f.getName), zipFile)
    zipFile
  }

  override lazy val projectSettings = inConfig(Compile)(scriptSettings)

}
