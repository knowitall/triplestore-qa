package edu.knowitall.eval


class LabelGenerator(oracle: Oracle, outputs: List[SystemOutput]) {
  
  def inputs = outputs.flatMap(_.inputs).distinct
  
  def topOutputs(input: String) = outputs.flatMap(_.topOutputFor(input)).distinct
  
  def inputOutputs = outputs.flatMap(_.inputOutputs).distinct
  
  def generateTopLabelFile = {
    for (i <- inputs; o <- topOutputs(i); if !oracle.hasLabel(i, o)) {
      printLabel(i, o)
    } 
  }
  
  def generateLabelFile = {
    for ((i, o) <- inputOutputs; if !oracle.hasLabel(i, o)) {
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
  val outputPaths = args.slice(2, args.size).toList
  val oracle = new FileOracle(labelsPath)
  val outputs = outputPaths map SystemOutput.fromPath
  val labeler = new LabelGenerator(oracle, outputs)
  mode match {
    case "top" => labeler.generateTopLabelFile
    case "all" => labeler.generateLabelFile
    case _ => throw new IllegalArgumentException("Usage: LabelGenerator [top|all] labels systemoutput") 
  }
  
}