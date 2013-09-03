import AssemblyKeys._ 

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

name := "triplestore-importer"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "commons-logging" % "commons-logging-api" % "1.0.4"

libraryDependencies += "edu.washington.cs.knowitall.openie" % "openie-models_2.10" % "1.1-SNAP2" exclude("com.esotericsoftware.minlog","minlog")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0"

libraryDependencies += "com.twitter" %% "util-collection" % "6.3.6"

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings