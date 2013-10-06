package edu.knowitall.paralex

import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.Scoobi._
import edu.knowitall.wikianswers.QuestionCluster
import com.nicta.scoobi.core.Reduction
import scala.io.Source

object TemplateCounterJob extends ScoobiApp {
  
  def lineToCluster(line: String): QuestionCluster = QuestionCluster.fromString(line)
  
  def getCounts(line: String): List[(String, Int)] = {
    val cluster = lineToCluster(line)
    val counter = new TemplateCounter(cluster)
    val pairs = for ((q1, q2) <- counter.templatePairs; List(qa, qb) = List(q1, q2).sortBy(x => x.toString())) yield (s"$qa|$qb", 1)
    val marginals = for (q <- counter.templateStrings) yield (s"$q", 1)
    (pairs.toList ++ marginals.toList).distinct
  }
  
  def run() = {
    val input = args(0)
    val thresh = args(1).toInt
    val output = args(2)
    val lines = fromTextFile(input)
    val counts = lines.mapFlatten(getCounts)
    val grouped = counts.groupByKey
    val summed = grouped.combine(Reduction.Sum.int)
    val filtered = summed.filter(x => x._2 >= thresh)
    val results = filtered.map(x => s"${x._1}\t${x._2}")
    persist(results.toTextFile(output, true))
  }

}

object TemplateCounterTest extends App {
  val lines = Source.fromInputStream(System.in, "UTF8").getLines
  lines.flatMap(TemplateCounterJob.getCounts) foreach println
}