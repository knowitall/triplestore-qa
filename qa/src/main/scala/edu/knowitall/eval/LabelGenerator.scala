package edu.knowitall.eval


class LabelGenerator(oracle: Oracle, output: SystemOutput) {
  
  def generateTopLabelFile = {
    for (i <- output.inputs; o <- output.topOutputFor(i); if !oracle.hasLabel(i, o)) {
      printLabel(i, o)
    } 
  }
  
  def generateLabelFile = {
    for ((i, o) <- output.inputOutputs; if !oracle.hasLabel(i, o)) {
      printLabel(i, o)
    }
  }
  
  def printLabel(input: String, output: String) = {
    println(s"LABEL\t0\t$input\t$output")
  }

}

object LabelGenerator extends App {
  val mode = args(0)
  val labelsPath = args(1)
  val outputPath = args(2)
  val oracle = new FileOracle(labelsPath)
  val output = SystemOutput.fromPath(outputPath)
  val labeler = new LabelGenerator(oracle, output)
  mode match {
    case "top" => labeler.generateTopLabelFile
    case "all" => labeler.generateLabelFile
    case _ => throw new IllegalArgumentException("Usage: LabelGenerator [top|all] labels systemoutput") 
  }
  
}