package edu.knowitall.eval.qa

import edu.knowitall.eval.Oracle
import edu.knowitall.eval.SystemOutput
import edu.knowitall.eval.FileOracle
import edu.knowitall.eval.LabelGenerator

class QASystemLabelGenerator(oracle: Oracle, outputs: List[QASystemOutput]) extends LabelGenerator(oracle, outputs) {
  
  def recordFor(i: String, o: String) = {
    val all = outputs.flatMap(_.qaRecordsFor(i, o)).distinct
    if (all.isEmpty) None else Some(all(0))
  }
  
  override def printLabel(in: String, out: String) = {
    println(s"LABEL\t0\t$in\t$out\n")
    for (r <- recordFor(in, out).take(1); d = r.derivation) {
      val line = d.replaceAll(" => ", "\n=>\n")
      println(line)
    }
    println
  }


}

object QASystemLabelGenerator extends App {
  val mode = args(0)
  val labelsPath = args(1)
  val outputPaths = args.slice(2, args.size).toList
  val oracle = new FileOracle(labelsPath)
  val outputs = outputPaths map QASystemOutput.fromPath
  val labeler = new QASystemLabelGenerator(oracle, outputs)
  mode match {
    case "top" => labeler.generateTopLabelFile
    case "all" => labeler.generateLabelFile
    case _ => throw new IllegalArgumentException("Usage: QASystemLabelGenerator [top|all] labels systemoutput") 
  }
  
}