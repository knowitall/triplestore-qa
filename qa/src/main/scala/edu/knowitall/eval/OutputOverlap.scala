package edu.knowitall.eval

import scala.io.Source

case class OverlapRow(input: String, outputs: List[Option[String]], labels: List[Boolean]) {
  assert(outputs.size == labels.size)
  override def toString = {
    val oStrs = outputs.map(_.getOrElse("No Answer"))
    val pStrs = oStrs.zip(labels) map {
      case (o, l) => if (o != "No Answer" && !l) s"*$o" else o
    }
    input + "\t" + pStrs.mkString("\t")
  }
}

object OutputOverlap extends App {
  
  def normalize(s: Option[String]) = s match {
    case Some(s) => Some(Oracle.normalize(s))
    case None => None
  }
  
  def overlapRow(input: String, outputs: List[SystemOutput], oracle: Oracle) = {
    val os = outputs.map(_.topOutputFor(input))
    val labels = os.map(o => o match {
    				     				case Some(o) => oracle.isCorrect(input, o)
    				     				case None => false
    				   			 	})
    val result = OverlapRow(input, os, labels)
    result
  }
  
  def overlapRows(inputs: List[String], outputs: List[SystemOutput], oracle: Oracle) = 
    inputs.map(overlapRow(_, outputs, oracle))
  
  def displayOverlapRows(rows: Iterable[OverlapRow]): String = rows.mkString("\n")
  
  override def main(args: Array[String]) = {
    val inputsPath = args(0)
    val labelsPath = args(1)
    val outputPaths = args.slice(2, args.size).toList
    val inputs = Source.fromFile(inputsPath, "UTF8").getLines.toList
    val oracle = new FileOracle(labelsPath)
    val outputs = outputPaths.map(SystemOutput.fromPath)
    val rows = overlapRows(inputs, outputs, oracle)
    println(displayOverlapRows(rows))
  }

}