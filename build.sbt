import com.typesafe.sbt.SbtStartScript

name := "market"

version := "0.1"

scalaVersion := "2.10.3"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.5"
                    
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.0.0-SNAPSHOT"

libraryDependencies += "joda-time" % "joda-time" % "2.3"

libraryDependencies += "org.joda" % "joda-convert" % "1.5"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

seq(SbtStartScript.startScriptForClassesSettings: _*)