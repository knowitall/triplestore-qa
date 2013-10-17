package edu.knowitall.eval

import scala.io.Source
import java.io.PrintWriter
import java.io.File
import com.typesafe.config.ConfigFactory

object Evaluator extends App {
  
  def evaluate(writer: PrintWriter, inputs: List[String], oracle: Oracle, output: SystemOutput) {
    var numCorrect = 0.0
    var numAnswered = 0.0
    for (i <- inputs; o <- output.topOutputFor(i)) {
      numAnswered += 1
      if (oracle.hasLabel(i, o)) {
      	if (oracle.isCorrect(i, o)) {
      	  numCorrect += 1
      	}
      } else {
        throw new IllegalArgumentException(s"No label for $i => $o")
      }
    }
    val precision = numCorrect / numAnswered
    val accuracy = numCorrect / inputs.size
    writer.println(s"${inputs.size} questions")
    writer.println(s"${numAnswered} answered")
    writer.println(s"${numCorrect} correct")
    writer.println(s"${precision} precision")
    writer.println(s"${accuracy} accuracy")
  }
  
  val inputsPath = args(0)
  val labelsPath = args(1)
  val outputPath = args(2)
  
  val conf = ConfigFactory.load()
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.toList
  val oracle = new FileOracle(labelsPath)
  val output = SystemOutput.fromPath(outputPath)
  val writer = new PrintWriter(new File(outputPath, conf.getString("eval.score.file")))
  evaluate(writer, inputs, oracle, output)
  writer.close()

}
