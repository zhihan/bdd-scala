lazy val root = (project in file(".")).
  settings(
    name := "bdd",
    version := "0.1",
    scalaVersion := "2.11.4",
    resolvers += "Sonatype Releases" at
      "http://oss.sonatype.org/contents/repositories/releases",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" %
        "2.1.7" % "test"
  )
