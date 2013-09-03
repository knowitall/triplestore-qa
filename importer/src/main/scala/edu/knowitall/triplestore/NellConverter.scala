package edu.knowitall.triplestore

import edu.knowitall.common.Resource.using
import java.io.File
import com.twitter.util.LruMap

object HumanFormat {
  
  val classInstancePattern = """\{"%%s([^"]*)%s",\s"([^"]*)"\}""".r
  val relationPattern = """\{"%%s([^"]*)%%s",\s"([^"]*)"\}""".r
  val tabRegex = "\\t".r
}

case class ClassInstanceFormat(val template: String) {
  
  import HumanFormat.classInstancePattern
  
  require(classInstancePattern.pattern.matcher(template).matches)
  
  def humanTriple(arg1: String) = template match {
    case classInstancePattern(relTemplate, insert) => {
      (arg1, relTemplate.trim, insert)
    }
    case _ => throw new RuntimeException("Couldn't match class-instance template")
  }
}

object ClassInstanceFormat {
  import HumanFormat._
  def fromOntology(file: File): Map[String, ClassInstanceFormat] = {
    using(io.Source.fromFile(file, "UTF8")) { source =>
      val ciformats = source.getLines.map(tabRegex.split).filter {
        case Array(_, "humanformat", template, _*) => classInstancePattern.pattern.matcher(template).matches
        case _ => false
      }
      ciformats.map { case Array(name, _, template, _*) => (name, ClassInstanceFormat(template)) } toMap
    }
  }
}

case class RelationFormat(val template: String) {
  
  import HumanFormat.relationPattern
  
  require(relationPattern.pattern.matcher(template).matches)
  
  def humanTriple(arg1: String, arg2: String) = template match {
    case relationPattern(relTemplate, insert) => {
      val relString = relTemplate.replaceAll("%s", insert).trim
      (arg1, relString, arg2)
    }
  }
}

object RelationFormat {
  import HumanFormat.{tabRegex, relationPattern}
  def fromOntology(file: File): Map[String, RelationFormat] = {
    using(io.Source.fromFile(file, "UTF8")) { source =>
      val ciformats = source.getLines.map(tabRegex.split).filter {
        case Array(_, "humanformat", template, _*) => relationPattern.pattern.matcher(template).matches
        case _ => false
      }
      ciformats.map { case Array(name, _, template, _*) => (name, RelationFormat(template)) } toMap
    }
  }
}

object NellConverter {
  
  import HumanFormat.tabRegex
  import java.io.PrintStream
  import scopt.OptionParser
  
  case class Config(ontologyFile: File = new File("."), kbFile: File= new File("."), output: PrintStream = System.out)

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("NellConverter") {
      arg[File]("ontologyFile") action { (f, c) => c.copy(ontologyFile = f) }

      arg[File]("kbFile") action { (f, c) => c.copy(kbFile = f) }

      opt[File]("output") optional () action { (f, c) => c.copy(output = new PrintStream(f)) }
    }
    
    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None => 
    }
  }
    
  def run(config: Config): Unit = {
    
    val classInstanceFormats = ClassInstanceFormat.fromOntology(config.ontologyFile)
    System.err.println(s"Loaded ${classInstanceFormats.size} class-instance formats")
    
    val relationFormats = RelationFormat.fromOntology(config.ontologyFile)
    System.err.println(s"Loaded ${relationFormats.size} relation formats")
    
    val kbSource = io.Source.fromFile(config.kbFile, "UTF8")
    
      val columnHeaders = Seq("Entity",
		  "Relation",
		  "Value",
		  "Iteration of Promotion",
		  "Probability",
		  "Source",
		  "Entity literalStrings",
		  "Value literalStrings",
		  "Best Entity literalString",
		  "Best Value literalString",
		  "Categories for Entity",
		  "Categories for Value",
		  "Candidate Source")

    val classInstanceRegex = "(concept:([^:]+)):.*".r
		  
    def tripleToTuple(triple: (String, String, String)): Map[String, String] = {
      Map(("arg1", triple._1), ("rel", triple._2), ("arg2", triple._3))
    }
    
    val isaCache = new LruMap[(String, String), Any](100)

    def lookupClassInstance(entity: String, literal: String): Option[Map[String, String]] = {
      if (isaCache.contains((entity, literal))) None
      else {
        isaCache.put((entity, literal), true)
        entity match {
          case classInstanceRegex(long, short) => {
            val ciFormatLookup = (classInstanceFormats.get(long) ++ classInstanceFormats.get(short)).headOption
            ciFormatLookup match {
              case Some(ciFormat) => Some(tripleToTuple(ciFormat.humanTriple(literal)))
              case None => None
            }
          }
          case _ => None
        }
      }
    }
		  
    def toTuples(kbLine: String): Seq[Map[String, String]] = {
      val split = tabRegex.split(kbLine)
      if (split.length < columnHeaders.length) Seq.empty
      else {
        val fieldMap = columnHeaders.zip(split).toMap
        val arg1 = fieldMap("Best Entity literalString")
        val arg2 = fieldMap("Best Value literalString")
        val (_, rel, _) = relationFormats.get(fieldMap("Relation")) match {
          case Some(relFormat) => relFormat.humanTriple(arg1, arg2)
          case None => ("", fieldMap("Relation").trim, "")
        }
        val tuple = Map(("arg1", arg1), ("rel", rel), ("arg2", arg2), ("prob_f", fieldMap("Probability")))
        Seq(tuple) ++ lookupClassInstance(fieldMap("Entity"), arg1)
      }
    }
    
    val stopRels = Set("generalizations")
    
    val tuples = kbSource.getLines.flatMap(toTuples)
    
    // throw out bad rels or tuples with empty fields
    val filteredTuples = tuples.filterNot(t => stopRels.contains(t("rel")) || t.iterator.exists(_._2.isEmpty))
    
    filteredTuples.zipWithIndex.foreach { case (t, index) =>
      if (index % 10000 == 0) System.err.println(index + " triples converted")
      val keyValueList = t.iterator.flatMap({ case (key, value) => Seq(key, value) })
      val lineString = keyValueList.mkString("\t")
      config.output.println(lineString)
    }
  }
}
