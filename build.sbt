name := "BlockAnalyzer"


version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.bitcoinj" % "bitcoinj-core" % "0.+",
  "org.mapdb" % "mapdb" % "2.+",
  "org.apache.commons" % "commons-math3" % "3.+"
)