package edu.knowitall.eval.qa

import scala.io.Source

object Evaluator extends App {
  
  def evaluate(questions: List[String], oracle: QAOracle, output: QASystemOutput) {
    var numCorrect = 0.0
    var numAnswered = 0.0
    for (q <- questions; a <- output.topAnswerFor(q)) {
      numAnswered += 1
      if (oracle.hasLabel(q, a)) {
      	if (oracle.isCorrect(q, a)) {
      	  numCorrect += 1
      	}
      } else {
        throw new IllegalArgumentException(s"No label for $q => $a")
      }
    }
    val precision = numCorrect / numAnswered
    val accuracy = numCorrect / questions.size
    println(s"${questions.size} questions")
    println(s"${numAnswered} answered")
    println(s"${numCorrect} correct")
    println(s"${precision} precision")
    println(s"${accuracy} accuracy")
  }
  
  val questionsPath = args(0)
  val labelsPath = args(1)
  val outputPath = args(2)
  
  val questions = Source.fromFile(questionsPath, "UTF8").getLines.toList
  val oracle = new FileQAOracle(labelsPath)
  val output = QASystemOutput.fromPath(outputPath)
  evaluate(questions, oracle, output)

}