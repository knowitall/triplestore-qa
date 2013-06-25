import AssemblyKeys._ 

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

name := "triplestore-query"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "commons-logging" % "commons-logging-api" % "1.0.4"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0"

assemblySettings
