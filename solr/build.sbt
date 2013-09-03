import AssemblyKeys._ 

net.virtualvoid.sbt.graph.Plugin.graphSettings

assemblySettings

scalaVersion := "2.10.2"

resolvers += "KnowItAll" at "http://knowitall.cs.washington.edu/maven2"

resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Restlet bullshit" at "http://maven.restlet.org/"
)

name := "triplestore-lemmatizer"

version := "0.1"

fork in run := true

javaOptions in run += "-Xmx8G"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "4.3.1"

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.3.1"

libraryDependencies += "edu.washington.cs.knowitall.common-scala" % "common-scala_2.10" % "1.1.1"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.2" exclude("com.github.scopt","scopt")

libraryDependencies += "com.twitter" %% "util-collection" % "6.3.6"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.rename
    case x => old(x)
  }
}

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

