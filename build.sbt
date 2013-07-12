import AssemblyKeys._ 

scalaVersion := "2.10.2"

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

name := "triplestore-qa"

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

libraryDependencies ++= Seq(
    "net.databinder" %% "unfiltered-filter" % "0.6.8",
    "net.databinder" %% "unfiltered-jetty" % "0.6.8",
    "com.jsuereth" %% "scala-arm" % "1.3",
    "com.github.scopt" %% "scopt" % "2.1.0",
    "ch.qos.logback" % "logback-classic" % "1.0.11",
    "ch.qos.logback" % "logback-core" % "1.0.11",
    "org.slf4j" % "slf4j-api" % "1.7.1"
)

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.rename
    case x => old(x)
  }
}

ivyXML := 
<dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
<artifact name="javax.servlet" type="orbit" ext="jar"/>
</dependency>
