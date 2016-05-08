lazy val commonSettings = Seq(
  name := "chee",
  version := "0.2.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val scalaLib = ExclusionRule("org.scala-lang", "scala-library")
lazy val slf4jApi = ExclusionRule("org.slf4j", "slf4j-api")

lazy val dependencies = Seq(
  "org.scalatest"              %% "scalatest"                % "2.2.4"    % "it,test",
  "org.scalacheck"             %% "scalacheck"               % "1.13.0"   % "test",
  "com.lihaoyi"                %% "fastparse"                % "0.3.7",
  "com.github.pathikrit"       %% "better-files"             % "2.15.0" excludeAll(
    scalaLib
  ),
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.4.0" excludeAll(
    ExclusionRule("org.scala-lang", "scala-reflect"),
    scalaLib,
    slf4jApi // use the one provided by logback
  ),
  "com.github.scopt"           %% "scopt"                    % "3.4.0",
  "com.sksamuel.scrimage"      %% "scrimage-core"            % "2.1.0" excludeAll(
    scalaLib,
    slf4jApi // use the one provided by logback
   ),
  "ch.qos.logback"              % "logback-classic"          % "1.1.7",
  "org.xerial"                  % "sqlite-jdbc"              % "3.8.11.2",
  "com.typesafe"                % "config"                   % "1.3.0",
  "org.bouncycastle"            % "bcpg-jdk15on"             % "1.54"
)

lazy val writeTestInfo = Def.task {
  val code = s"""package chee
    | import better.files._
    | object TestInfo {
    |  val baseDir = file"${baseDirectory.value}"
    |  val targetDir = file"${(target in Test).value}"
    |  val resourceDir = file"${(resourceDirectory in Test).value}"
    |  val gnupgDir = resourceDir / "gnupg"
    |  val images: List[File] =
    |    ${IO.listFiles((resourceDirectory in Test).value / "images").map( f => "resourceDir / \"images\" / \"" + f.getName + "\"").toList}
    |  val sampleDb = resourceDir / "sample.db"
    |}
    """.stripMargin
  val file = (sourceManaged in Test).value / "chee" / "TestInfo.scala"
  IO.write(file, code)
  Seq(file)
}

lazy val testSettings = Defaults.itSettings ++ Seq(
  fork in Test := true,
  fork in IntegrationTest := true,
  javaOptions in Test ++= Seq(
    "-Duser.timezone=Europe/Berlin",
    s"-Dchee.workingdir=${(target in Test).value}/test",
    s"-Dchee.configdir=${(target in Test).value}/test",
    s"-Duser.dir=${(target in Test).value}/test",
    "-Dchee.logLevel=OFF"
  ),
  javaOptions in IntegrationTest := Seq(
    "-Duser.timezone=Europe/Berlin"
  ),
  sourceGenerators in Test += writeTestInfo.taskValue,
  sourceGenerators in IntegrationTest += writeTestInfo.taskValue
)

lazy val buildSettings = Seq(
  libraryDependencies := dependencies,
  resourceGenerators in Compile += (listDocResources in CheeDoc).taskValue,
  sourceGenerators in Compile += (genDocInfo in CheeDoc).taskValue
)

addCommandAlias("make-chee", ";genDocResources;gen-chee")
addCommandAlias("make-zip", ";genDocResources;test;it:test;gen-zip")

lazy val chee = (project in file("."))
  .configs(IntegrationTest, CheeDoc, script)
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .settings(buildSettings: _*)
