package edu.knowitall.paralex

import com.nicta.scoobi.application.ScoobiApp
import com.nicta.scoobi.Scoobi._
import edu.knowitall.wikianswers.QuestionCluster
import com.nicta.scoobi.core.Reduction

object TemplateCounterJob extends ScoobiApp {
  
  def lineToCluster(line: String): QuestionCluster = QuestionCluster.fromString(line)
  
  def getCounts(line: String): List[(String, Int)] = {
    System.err.prinln(line.slice(0, 80))
    val cluster = lineToCluster(line)
    val counter = new TemplateCounter(cluster)
    val pairs = for ((q1, q2) <- counter.templatePairs) yield (s"$q1|$q2", 1)
    val marginals = for (q <- counter.templateStrings) yield (s"$q", 1)
    (pairs.toList ++ marginals.toList).distinct
  }
  
  def run() = {
    val input = args(0)
    val output = args(1)
    val lines = fromTextFile(input)
    val results = lines.mapFlatten(getCounts).groupByKey.combine(Reduction.Sum.int).filterNot(x => x._2 < 10).map(x => s"${x._1}\t${x._2}")
    persist(results.toTextFile(output, true))
  }

}