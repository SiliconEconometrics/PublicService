name := "CountPreferentialVotes"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

// libraryDependencies += "junit" % "junit" % "4.11" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test" // junit is pulled in as a transitive dependency.

libraryDependencies += "com.google.code.gson" % "gson" % "2.8.1"

libraryDependencies += "org.jsoup" % "jsoup" % "1.9.1" // parse xml

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "4.1.2" // parse xslx files

scalacOptions ++= Seq("-deprecation", "-feature")

mainClass in assembly := Some("org.greatcactus.vote.count.MainApp")

test in assembly := {}

