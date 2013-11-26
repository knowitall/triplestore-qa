import AssemblyKeys._

import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

name := "triplestore-importer"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "commons-logging" % "commons-logging-api" % "1.0.4"

libraryDependencies += "edu.washington.cs.knowitall.openie" % "openie-models_2.10" % "1.1-SNAPSHOT" exclude("com.esotericsoftware.minlog","minlog")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0"

libraryDependencies += "com.twitter" %% "util-collection" % "6.3.6"

libraryDependencies += "edu.washington.cs.knowitall.openie" %% "openie-populator" % "1.1-SNAPSHOT"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-stanford" % "2.4.4"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-clear" % "2.4.4"

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings
