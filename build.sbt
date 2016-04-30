lazy val commonSettings = Seq(
  name := "chee",
  version := "0.2.0",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val dependencies = Seq(
  "org.scalatest"              %% "scalatest"                % "2.2.4"    % "it,test",
  "org.scalacheck"             %% "scalacheck"               % "1.12.5"   % "test",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.4",
  "com.github.pathikrit"       %% "better-files"             % "2.14.0",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.1.0",
  "com.github.scopt"           %% "scopt"                    % "3.4.0",
  "com.sksamuel.scrimage"      %% "scrimage-core"            % "2.1.0",
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
addCommandAlias("make-zip", ";genDocResources;test;gen-zip")

lazy val chee = (project in file("."))
  .configs(IntegrationTest, CheeDoc, script)
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .settings(buildSettings: _*)
