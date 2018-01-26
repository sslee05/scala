import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies ++= Seq(
        scalaTest % Test,
        //S:akka lib
        "com.typesafe.akka" %% "akka-actor" % "2.5.9",
        "com.typesafe.akka" %% "akka-testkit" % "2.5.9" % Test,
        "com.typesafe.akka" %% "akka-stream" % "2.5.9",
        "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.9" % Test,
        "com.typesafe.akka" %% "akka-http" % "10.0.11",
        "com.typesafe.akka" %% "akka-http-testkit" % "10.0.11" % Test,
        "com.typesafe.akka" %% "akka-cluster" % "2.5.9",
        "com.typesafe.akka" %% "akka-cluster-sharding" %  "2.5.9",
        "com.typesafe.akka" %% "akka-distributed-data" % "2.5.9",
        //E:akka lib
		
		//S:ScalaCheck
		"org.scalacheck" % "scalacheck_2.12" % "1.13.5" % "test"
		//E:ScalaCheck
    )

  )
