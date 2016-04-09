name := """selfservice"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Local Maven Repository" at Path.userHome.asFile.toURI.toURL + ".m2/repository"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.akka23-SNAPSHOT",
  "com.walcron" % "utilities-scala_2.11" % "1.0.0",
  "com.walcron" % "forecast-io-api_2.10" % "0.0.1",
  "com.google.http-client" % "google-http-client" % "1.18.0-rc",
  "com.google.http-client" % "google-http-client-jackson2" % "1.18.0-rc",
  "com.google.apis" % "google-api-services-oauth2" % "v2-rev70-1.18.0-rc",
  "javax.mail" % "mail" % "1.4",
  "com.wordnik" %% "swagger-play2" % "1.3.10",
  "io.lamma" %% "lamma" % "2.2.0",
  "org.apache.poi" % "poi" % "3.11",
  "org.apache.poi" % "poi-ooxml" % "3.11",
  "org.imgscalr" % "imgscalr-lib" % "4.2"
)

scalacOptions += "-feature"
