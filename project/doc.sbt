libraryDependencies += "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.1"

libraryDependencies ++= Seq(
  //stringtemplate has circular dep: st -> antlr-runtime -> st
  "org.antlr" % "stringtemplate" % "4.0.2" exclude(
    "org.antlr", "antlr-runtime"
  ),
  "org.antlr" % "antlr-runtime" % "3.3" exclude (
    "org.antlr", "stringtemplate"
  )
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
