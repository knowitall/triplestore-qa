package edu.knowitall.eval

import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.util.Random

object BootstrapF1 extends App {
  
  def bootstrapSample[T](seq: IndexedSeq[T]): IndexedSeq[T] = {
    val n = seq.size
    val result = for (i <- 1 to n; k = Random.nextInt(n)) yield seq(k)
    result
  }
  
  def predictions(inputs: List[String], output: SystemOutput, oracle: FileOracle): Map[String, Option[Double]] = {
    val pairs = for {
      i <- inputs
      oo = output.topOutputFor(i) match {
        case Some(o) if oracle.isCorrect(i, o) => Some(1.0)
        case Some(o) if !oracle.isCorrect(i, o) => Some(0.0)
        case None => None
      } 
    } yield (i, oo)
    pairs.toMap
  }
  
  def f1(inputs: Iterable[String], preds: Map[String, Option[Double]]): Double = {
    val gotPreds = for {
      i <- inputs
      o <- preds(i)
    } yield o
    val numCorrect = gotPreds.reduce(_ + _)
    val numPred = gotPreds.size.toDouble
    val recallDenom = inputs.size.toDouble
    if (recallDenom > 0 && numPred > 0) {
      val precision = numCorrect / numPred
      val recall = numCorrect / recallDenom
      2 * precision * recall / (precision + recall)
    } else {
      0.0
    }
  }
  
  def bootstrapF1(inputs: Iterable[String], output: SystemOutput, oracle: FileOracle, numSamples: Int) = {
    val preds = predictions(inputs.toList, output, oracle)
    val inputSeq = inputs.toIndexedSeq
    val f1s = for {
      i <- 1 to numSamples
      sample = bootstrapSample(inputSeq)
      sampleF1 = f1(sample, preds)
    } yield sampleF1
    f1s
  }
  
  val inputsPath = args(0)
  val labelsPath = args(1)
  val numSamples = args(2).toInt
  val outputPaths = args.slice(3, args.size).toList
  
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.toList
  val oracle = new FileOracle(labelsPath)
  
  for (outputPath <- outputPaths) {
    println(s"Sampling for $outputPath")
    val output = SystemOutput.fromPath(outputPath)
    val f1s = bootstrapF1(inputs, output, oracle, numSamples)
    val writer = new PrintWriter(new File(outputPath, "bootstrap-f1.txt"))
    writer.println(f1s.mkString("\n"))
    writer.close()
  }

}