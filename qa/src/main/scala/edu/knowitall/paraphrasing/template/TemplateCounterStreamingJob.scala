package edu.knowitall.paraphrasing.template

import scala.io.Source
import edu.knowitall.util.HadoopUtils


object TemplateCounterStreamingJob extends App {
  val lines = Source.fromInputStream(System.in, "UTF8").getLines
  for (line <- lines; (key, value) <- TemplateCounterJob.getCounts(line)) println(s"$key\t$value")
}