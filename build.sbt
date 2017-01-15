name := "twitter-responses"

scalaVersion := "2.11.7"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.typesafe"  %  "config"         % "1.2.1",
  "org.twitter4j" %  "twitter4j-core" % "4.0.6",
  "org.scalatest" %% "scalatest"      % "2.2.6" % "test" 
)

