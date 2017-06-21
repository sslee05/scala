import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies ++= Seq(
        scalaTest % Test,
        //S:akka lib
        "com.typesafe.akka" %% "akka-actor" % "2.5.2",
        "com.typesafe.akka" %% "akka-testkit" % "2.5.2" % Test,
        "com.typesafe.akka" %% "akka-stream" % "2.5.2",
        "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.2" % Test,
        "com.typesafe.akka" %% "akka-http" % "10.0.7",
        "com.typesafe.akka" %% "akka-http-testkit" % "10.0.7" % Test,
        "com.typesafe.akka" %% "akka-cluster" % "2.5.2",
        "com.typesafe.akka" %% "akka-cluster-sharding" %  "2.5.2",
        "com.typesafe.akka" %% "akka-distributed-data" % "2.5.2"
        //E:akka lib
    )

  )
