package edu.knowitall.eval

import com.typesafe.config.ConfigFactory
import scala.io.Source
import java.io.PrintWriter
import java.io.File

object PrecisionRecall extends App {
  
  def bool2int(b:Boolean) = if (b) 1 else 0
  
  def precisionRecall(predictions: List[Int], recallDenom: Double): List[(Double, Double)] = {
    val numCorrect = predictions.scanLeft(0.0)(_ + _).tail
    val precs = numCorrect.zipWithIndex.map(x => x._1 / (x._2 + 1))
    val recs = numCorrect.zipWithIndex.map(x => x._1 / recallDenom)
    recs zip precs  
  }
  
  def topCorrect(i: String, oracle: Oracle, output: SystemOutput): (Double, Boolean) = output.topOutputFor(i) match {
    case Some(o) => (output.topScoreFor(i, o).get, oracle.isCorrect(i, o))
    case None => (Double.MinValue, false)
  }
  
  def computeTopPr(writer: PrintWriter, inputs: List[String], oracle: Oracle, output: SystemOutput) = {
    val preds = inputs.map(i => topCorrect(i, oracle, output)).sortBy(x => -x._1).map(x => bool2int(x._2))
    val denom = inputs.size
    for ((r, p) <- precisionRecall(preds, denom)) writer.println(s"$r\t$p")
  }
  
  def computePr(writer: PrintWriter, inputs: List[String], oracle: Oracle, output: SystemOutput) = {
    val scoredPreds = for (r <- output.records; i = r.input; o = r.output; score = r.score; corr = oracle.isCorrect(i, o)) yield (score, corr)
    val preds = scoredPreds.sortBy(-_._1).map(x => bool2int(x._2))
    val denom = preds.sum
    for ((r, p) <- precisionRecall(preds, denom)) writer.println(s"$r\t$p")
  }
  
  val mode = args(0)
  val inputsPath = args(1)
  val labelsPath = args(2)
  val outputPath = args(3)
  
  val conf = ConfigFactory.load()
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.toList
  val oracle = new FileOracle(labelsPath)
  val output = SystemOutput.fromPath(outputPath)
  val writer = new PrintWriter(new File(outputPath, conf.getString("eval.pr.file")))
  mode match {
    case "top" => computeTopPr(writer, inputs, oracle, output)
    case "all" => computePr(writer, inputs, oracle, output)
  }
  writer.close()
  

}