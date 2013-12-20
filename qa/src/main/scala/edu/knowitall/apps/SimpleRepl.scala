package edu.knowitall.apps

import jline.console.ConsoleReader
import edu.knowitall.model.QaModel
import edu.knowitall.model.Derivation

class SimpleRepl {
  val model = QaModel()
  def result(d: Derivation) = {
    s"${d.answer}\n${d.score}\n$d\n${d.explainScore(model.costModel.weights)}\n"
  }
  def eval(input: String) = {
    val candidates = model.candidatePredictions(input)
    candidates map result mkString("\n")
  }
}

object SimpleRepl extends App {
  
  override def main(args: Array[String]) = {
    import jline.console.ConsoleReader
    val reader = new ConsoleReader()
    val repl = new SimpleRepl
    while (true) {
	  val line = reader.readLine("> ")
	  val result = repl.eval(line)
  	  println(result)
    }
  }

}