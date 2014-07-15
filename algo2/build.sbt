name := "algo1"

version := "1.0.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Sonatype OSS scala-tools" at "https://oss.sonatype.org/content/repositories/scala-tools"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"