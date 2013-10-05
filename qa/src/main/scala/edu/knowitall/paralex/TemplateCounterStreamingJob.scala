package edu.knowitall.paralex

import scala.io.Source
import edu.knowitall.wikianswers.QuestionCluster
import edu.knowitall.util.HadoopUtils

object TemplateCounterStreamingJob extends App {
  
  def mapJob {
    val lines = Source.fromInputStream(System.in, "UTF8").getLines
    for (line <- lines; (key, value) <- TemplateCounterJob.getCounts(line)) println(s"$key\t$value")
  }
  
  def reduceJob {
    val minCount = 0
    val lines = Source.fromInputStream(System.in, "UTF8").getLines
    val pairs = lines.map(_.split("\t", 2)).map { case Array(a, b) => (a, b.toInt)}
    def getKey(x: (String, Int)) = x._1
    val groups = HadoopUtils.groupIterator(pairs, getKey)
    val totaled = for ((k, rest) <- groups; restInt = rest.map(_._2); total = restInt.fold(0)((a, b) => a + b); if total >= minCount) yield (k, total)
    totaled.foreach(x => println(s"${x._1}\t${x._2}"))
  }
  
  val mode = args(0)
  mode match {
    case "map" => mapJob
    case "reduce" => reduceJob
    case _ => throw new IllegalArgumentException("must specify map or reduce")
  }

}