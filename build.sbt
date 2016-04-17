name := "chee"

version := "0.2.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

fork in Test := true

javaOptions in Test ++= Seq(
  "-Duser.timezone=Europe/Berlin",
  "-Dchee.workingdir=target",
  "-Dchee.configdir=target",
  "-Dchee.logLevel=OFF"
)

resourceGenerators in Compile += (listDocResources in CheeDoc).taskValue

sourceGenerators in Compile += (genDocInfo in CheeDoc).taskValue

sourceGenerators in Test += task {
  val code = s"""package chee
import better.files._
object TestInfo {
  val baseDir = file"${baseDirectory.value}"
  val targetDir = file"${(target in Test).value}"
  val resourceDir = file"${(resourceDirectory in Test).value}"
  val gnupgDir = resourceDir / "gnupg"
  val images: List[File] =
    ${IO.listFiles((resourceDirectory in Test).value / "images").map( f => "resourceDir / \"images\" / \"" + f.getName + "\"").toList}
  val sampleDb = resourceDir / "sample.db"
}
"""
  val file = (sourceManaged in Test).value / "chee" / "TestInfo.scala"
  IO.write(file, code)
  Seq(file)
}

addCommandAlias("make-chee", ";genDocResources;gen-chee")

addCommandAlias("make-zip", ";genDocResources;test;gen-zip")

libraryDependencies ++= Seq(
  "org.scalatest"              %% "scalatest"                % "2.2.4"    % "test",
  "org.scalacheck"             %% "scalacheck"               % "1.12.5"   % "test",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.4",
  "com.github.pathikrit"       %% "better-files"             % "2.14.0",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.1.0",
  "com.github.scopt"           %% "scopt"                    % "3.4.0",
  "com.sksamuel.scrimage"      %% "scrimage-core"            % "2.1.0",
  "ch.qos.logback"              % "logback-classic"          % "1.1.3",
  "org.xerial"                  % "sqlite-jdbc"              % "3.8.11.2",
  "com.typesafe"                % "config"                   % "1.3.0",
  "org.bouncycastle"            % "bcpg-jdk15on"             % "1.54"
)
