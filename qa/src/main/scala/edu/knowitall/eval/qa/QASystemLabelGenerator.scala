package edu.knowitall.eval.qa

import edu.knowitall.eval.Oracle
import edu.knowitall.eval.SystemOutput
import edu.knowitall.eval.FileOracle
import edu.knowitall.eval.LabelGenerator

class QASystemLabelGenerator(oracle: Oracle, output: QASystemOutput) extends LabelGenerator(oracle, output) {
  
  override def printLabel(in: String, out: String) = {
    println(s"LABEL\t0\t$in\t$out\n")
    for (r <- output.records; d = r.derivation) {
      val line = d.replaceAll(" => ", "\n=>\n")
      println(line)
    }
    println
  }


}

object QASystemLabelGenerator extends App {
  val mode = args(0)
  val labelsPath = args(1)
  val outputPath = args(2)
  val oracle = new FileOracle(labelsPath)
  val output = QASystemOutput.fromPath(outputPath)
  val labeler = new QASystemLabelGenerator(oracle, output)
  mode match {
    case "top" => labeler.generateTopLabelFile
    case "all" => labeler.generateLabelFile
    case _ => throw new IllegalArgumentException("Usage: QASystemLabelGenerator [top|all] labels systemoutput") 
  }
  
}