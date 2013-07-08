import AssemblyKeys._ 

scalaVersion := "2.10.2"

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

name := "triplestore-query"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "commons-logging" % "commons-logging-api" % "1.0.4"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0"

libraryDependencies += "com.rockymadden.stringmetric" % "stringmetric-core" % "0.25.3"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "4.3.1"

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.3.1"

libraryDependencies += "jline" % "jline" % "2.11"

libraryDependencies += "edu.washington.cs.knowitall.common-scala" % "common-scala_2.10" % "1.1.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP26"

assemblySettings
