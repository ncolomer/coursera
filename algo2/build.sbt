name := "algo1"

version := "1.0.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Sonatype OSS scala-tools" at "https://oss.sonatype.org/content/repositories/scala-tools"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"