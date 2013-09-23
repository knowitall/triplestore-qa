package edu.knowitall.paralex

import scala.io._
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.Scoobi.ScoobiApp
import com.nicta.scoobi.io.text.TextInput
import com.nicta.scoobi.io.text.TextSource
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.core.DList

object GenerateTemplates extends ScoobiApp {
  
  val generator = TemplateGenerator()
  
  def lineToTemplates(line: String): List[String] = {
    val c = QACluster.fromString(line)
    generator.generateTemplates(c).map(_.toString)
  }
  
  def run() {
    val lines = TextInput.fromTextSource(new TextSource(Seq(args(0))))
    val clusters = lines.mapFlatten(lineToTemplates)
    persist(clusters.toTextFile(args(1), true))    
  }
  
}

object GenerateTemplatesCLI extends App {
  val generator = TemplateGenerator()
  def lineToTemplates(line: String): List[String] = {
    val c = QACluster.fromString(line)
    generator.generateTemplates(c).map(_.toString)
  }
  
  val input = Source.fromInputStream(System.in, "UTF8").getLines
  if (args.size > 0 && args(0) == "threaded") {
	System.err.println("Running in threaded mode")
  	val groups = input.grouped(100)
  	groups.foreach { lines => 
  		lines.par.flatMap(lineToTemplates).foreach(println)
  	}
  } else {
    input.flatMap(lineToTemplates).foreach(println)
  }
}