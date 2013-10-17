package edu.knowitall.eval.qa

import edu.knowitall.apps.QASystem
import java.io.File
import java.io.IOException
import scala.io.Source

class QASystemRunner(qa: QASystem, path: String) {

  val outputFile = new File(path)
  if (!outputFile.exists()) {
    outputFile.mkdirs()
  }
  if (outputFile.exists() && !outputFile.isDirectory()) {
    throw new IOException(s"Could not write to $path, file exists")
  }
  
  def runFile(path: String) = {
    val lines = Source.fromFile(path, "UTF8").getLines.toList
    run(path, lines)
  }
  
  def run(name: String, questions: List[String]) = {
    val records = for (q <- questions;
    				   group <- qa.answer(q);
    				   r = SystemOutputRecord.fromScoredAnswerGroup(q, group))
    				yield r
    val output = new QASystemOutput(path, records, name)
    output.save
  }
  
}

object QASystemRunner extends App {
  val input = args(0)
  val output = args(1)
  val qa = QASystem.getInstance().get
  val runner = new QASystemRunner(qa, output)
  runner.runFile(input)
}