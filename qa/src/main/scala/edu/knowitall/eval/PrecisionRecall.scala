package edu.knowitall.eval

import com.typesafe.config.ConfigFactory
import scala.io.Source
import java.io.PrintWriter
import java.io.File

object PrecisionRecall extends App {
  
  def bool2int(b:Boolean) = if (b) 1 else 0
  
  def precisionRecall(predictions: List[Int]): List[(Double, Double)] = {
    val recallDenom = predictions.sum
    val numCorrect = predictions.scanLeft(0.0)(_ + _).tail
    val precs = numCorrect.zipWithIndex.map(x => x._1 / (x._2 + 1))
    val recs = numCorrect.zipWithIndex.map(x => x._1 / recallDenom)
    recs zip precs  
  }
  
  def computePr(writer: PrintWriter, inputs: List[String], oracle: Oracle, output: SystemOutput) = {
    val scoredPreds = for (r <- output.records; i = r.input; o = r.output; score = r.score; corr = oracle.isCorrect(i, o)) yield (score, corr)
    val preds = scoredPreds.sortBy(-_._1).map(x => bool2int(x._2))
    for ((r, p) <- precisionRecall(preds)) writer.println(s"$r\t$p")
  }
  
  val inputsPath = args(0)
  val labelsPath = args(1)
  val outputPath = args(2)
  
  val conf = ConfigFactory.load()
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.toList
  val oracle = new FileOracle(labelsPath)
  val output = SystemOutput.fromPath(outputPath)
  val writer = new PrintWriter(new File(outputPath, conf.getString("eval.pr.file")))
  computePr(writer, inputs, oracle, output)
  writer.close()
  

}