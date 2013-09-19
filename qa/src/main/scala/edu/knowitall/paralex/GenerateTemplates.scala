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
  val lines = Source.fromInputStream(System.in, "UTF8").getLines
  lines.flatMap(lineToTemplates).foreach(println)
}