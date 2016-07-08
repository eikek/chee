lazy val commonSettings = Seq(
  name := "chee",
  homepage := Some(url("https://github.com/eikek/chee")),
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val scalaLib = ExclusionRule("org.scala-lang", "scala-library")
lazy val slf4jApi = ExclusionRule("org.slf4j", "slf4j-api")

lazy val dependencies = Seq(
  "org.scalatest"              %% "scalatest"                % "2.2.6"    % "test",
  "org.scalacheck"             %% "scalacheck"               % "1.13.1"   % "test",
  "com.lihaoyi"                %% "fastparse"                % "0.3.7",
  "com.github.pathikrit"       %% "better-files"             % "2.16.0" excludeAll(
    scalaLib
  ),
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.4.0" excludeAll(
    ExclusionRule("org.scala-lang", "scala-reflect"),
    scalaLib,
    slf4jApi // use the one provided by logback
  ),
  "com.github.scopt"           %% "scopt"                    % "3.5.0",
  "com.sksamuel.scrimage"      %% "scrimage-core"            % "2.1.6" excludeAll(
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
    |  /** Repo dir is /home/eike/workspace */
    |  val sampleRepoDb = resourceDir / "sampleRepo.db"
    |}
    """.stripMargin
  val file = (sourceManaged in Test).value / "chee" / "TestInfo.scala"
  IO.write(file, code)
  Seq(file)
}

lazy val ItTest = config("itTest") extend Test
def itFilter(name: String): Boolean = (name endsWith "Test") && (name.startsWith("chee.cli")  || name.startsWith("chee.it"))
def unitFilter(name: String): Boolean = ((name endsWith "Test") || (name endsWith "Spec")) && !itFilter(name)

lazy val testSettings = Defaults.itSettings ++ Seq(
  fork in Test := true,
  fork in ItTest := true,
  javaOptions in Test ++= Seq(
    "-Duser.timezone=Europe/Berlin",
    s"-Dchee.workingdir=${(target in Test).value}/test/.chee-dir",
    s"-Dchee.configdir=${(target in Test).value}/test/.chee-dir",
    s"-Duser.dir=${(target in Test).value}/test"
  ),
  testOptions in Test := Seq(Tests.Filter(unitFilter)),
  testOptions in ItTest := Seq(Tests.Filter(itFilter)),
  sourceGenerators in Test += writeTestInfo.taskValue
)

lazy val bootstrapVersion = "3.3.6"
lazy val galleryVersion = "2.21.2"
lazy val bootstrapGalleryVersion = "3.4.2"
lazy val jqueryVersion = "2.2.4"
lazy val fontAwesomeVersion = "4.6.3"
lazy val markedVersion = "0.3.5"

lazy val themes = List("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "lumen",
  "paper", "readable", "sandstone", "simplex", "slate", "spacelab", "superhero", "united", "yeti")

lazy val buildSettings = Seq(
  libraryDependencies := dependencies,
  resourceLibs in Compile ++= Seq(
    // js
    Lib.js(jqueryVersion, "http://code.jquery.com/jquery-<version>.min.js"),
    Lib.js(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/js/jquery.blueimp-gallery.min.js"),
    Lib.js(bootstrapGalleryVersion, "https://github.com/blueimp/Bootstrap-Image-Gallery/raw/v<version>/js/bootstrap-image-gallery.min.js"),
    Lib.js(markedVersion, "https://github.com/chjj/marked/raw/v<version>/marked.min.js"),
    //css
    Lib.css(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/css/font-awesome.min.css"),
    Lib.css(bootstrapVersion, "http://netdna.bootstrapcdn.com/bootstrap/<version>/css/bootstrap.min.css").in("bootstrap"),
    Lib.css(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/css/blueimp-gallery.min.css"),
    Lib.css(bootstrapGalleryVersion, "https://github.com/blueimp/Bootstrap-Image-Gallery/raw/v<version>/css/bootstrap-image-gallery.min.css"),
    // fonts
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/FontAwesome.otf"),
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/fontawesome-webfont.eot"),
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/fontawesome-webfont.svg"),
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/fontawesome-webfont.ttf"),
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/fontawesome-webfont.woff"),
    Lib.font(fontAwesomeVersion, "https://maxcdn.bootstrapcdn.com/font-awesome/<version>/fonts/fontawesome-webfont.woff2"),
    // img
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/error.png"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/error.svg"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/play-pause.png"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/play-pause.svg"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/loading.gif"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/video-play.png"),
    Lib.img(galleryVersion, "https://github.com/blueimp/Gallery/raw/v<version>/img/video-play.svg")
  ) ++ themes.map(theme => Lib.css(bootstrapVersion, s"https://maxcdn.bootstrapcdn.com/bootswatch/<version>/${theme}/bootstrap.min.css").in(theme)),
  resourceGenerators in Compile += (listDocResources in CheeDoc).taskValue,
  resourceGenerators in Compile += (fetchResources in Compile).taskValue,
  sourceGenerators in Compile += (genDocInfo in CheeDoc).taskValue,
  sourceGenerators in Compile += (resourceInfo in Compile).taskValue.map(f => Seq(f))
)

addCommandAlias("run-all-tests", ";genDocResources;test;itTest:test")
addCommandAlias("make-chee", ";genDocResources;gen-chee")
addCommandAlias("make-zip", ";run-all-tests;gen-zip;gen-chee")

lazy val chee = (project in file("."))
  .configs(ItTest, CheeDoc, script)
  .settings(inConfig(ItTest)(Defaults.testTasks): _*)
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .settings(buildSettings: _*)
