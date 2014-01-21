import AssemblyKeys._

import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

scalaVersion := "2.10.2"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

resolvers += "ClouderaRepo" at "https://repository.cloudera.com/content/repositories/releases"

resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Restlet bullshit" at "http://maven.restlet.org/"
)

organization := "edu.knowitall.triplestore-qa"

name := "triplestore-qa"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

fork in run := true

javaOptions in run += "-Xmx8G"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "4.3.0"

libraryDependencies += "org.apache.solr" % "solr" % "4.3.1"

libraryDependencies += "org.apache.solr" % "solr-core" % "4.3.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0"

libraryDependencies += "com.rockymadden.stringmetric" % "stringmetric-core" % "0.25.3"

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.3.1"

libraryDependencies += "com.google.guava" % "guava" % "16.0-rc1"

libraryDependencies += "jline" % "jline" % "2.11"

libraryDependencies += "edu.washington.cs.knowitall.common-scala" % "common-scala_2.10" % "1.1.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP26"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.5"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.5-SNAPSHOT" exclude("com.github.scopt","scopt")

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.10"

libraryDependencies += "edu.berkeley.nlp" % "berkeleylm" % "1.1.2"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-clear" % "2.4.5-SNAPSHOT" exclude("com.github.scopt","scopt")

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-breeze" % "2.4.5-SNAPSHOT" exclude("com.github.scopt","scopt")

libraryDependencies += "com.twitter" %% "util-collection" % "6.3.6"

libraryDependencies += "com.clearspring.analytics" % "stream" % "2.5.1"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

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

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.5-SNAPSHOT"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.9"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-chunk-opennlp" % "2.4.5-SNAPSHOT"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-stanford" % "2.4.5-SNAPSHOT"

libraryDependencies += "com.nicta" % "scoobi_2.10" % "0.7.0-RC2-cdh3"

libraryDependencies += "org.apache.hadoop" % "hadoop-lzo" % "0.4.13"

libraryDependencies += "com.typesafe" % "config" % "1.0.2"

//libraryDependencies += "edu.washington.cs.knowitall" % "openregex-scala_2.10" % "1.0.4"

libraryDependencies += "edu.washington.cs.knowitall.taggers" % "taggers-core_2.10" % "0.4"

libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.3"

libraryDependencies += "edu.berkeley.nlp" % "berkeleylm" % "1.1.2"

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

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.1" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x => {
      val oldstrat = old(x)
      if (oldstrat == MergeStrategy.deduplicate) MergeStrategy.first
      else oldstrat
    }
  }
}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {c=> List("scalatest", "lucene", "shapeless", "solr") exists {c.data.getName contains _} }
}
