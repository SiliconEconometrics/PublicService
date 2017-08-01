name := "CountPreferentialVotes"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

libraryDependencies += "junit" % "junit" % "4.11" % Test

libraryDependencies += "com.google.code.gson" % "gson" % "2.8.1"

libraryDependencies += "org.jsoup" % "jsoup" % "1.9.1"

scalacOptions ++= Seq("-deprecation", "-feature")

