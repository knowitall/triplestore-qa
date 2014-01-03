package edu.knowitall.apps

import jline.console.ConsoleReader
import edu.knowitall.model.QaModel
import edu.knowitall.model.Derivation
import edu.knowitall.parsing.cg.CgParserRepl
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.search.qa.QueryState
import edu.knowitall.search.qa.TupleState
import edu.knowitall.execution.Tabulator
import edu.knowitall.execution.Search
import edu.knowitall.search.qa.QuestionState
import edu.knowitall.search.qa.QaStep

class SimpleRepl {
  val model = QaModel()
  var evalFn = qaEval _
  val evalFns = Map(
    "qa" -> qaEval _,
    "parser" -> parserEval _,
    "query" -> queryEval _,
    "paraphrase" -> paraphraseEval _,
    "relsyn" -> relSynEval _
  )
  val parserRepl = new CgParserRepl
  val exec = model.transitionModel.executeTransition
  val absArg = model.transitionModel.absArgTransition
  val templ = model.transitionModel.templateTransition
  val relSyn = model.transitionModel.relSynTransition
  val cost = model.costModel
  
  private def qaEval(input: String) = {
    val candidates = model.candidatePredictions(input)
    candidates map result mkString("\n")
  }
  
  private def queryState(input: String) = ListConjunctiveQuery.fromString(input) match {
      case Some(x) => QueryState(x)
      case None => throw new IllegalArgumentException(s"Could not parse '$input'")
    }
  
  private def relSynEval(input: String) = {
    val fromState = queryState(input)
    val steps = for ((action, toState) <- relSyn(fromState)) 
      yield QaStep(input, fromState, action, toState)
    val lines = steps.toList.sortBy(cost(_)) map {
      step => s"${step.toState} ${-cost(step)}"
    }
    lines.mkString("\n")
  }
  
  private def parserEval(input: String) = parserRepl.eval(input)
  
  private def queryEval(input: String) = {
    val state = queryState(input)
    val tuples = for {
      (action, newState) <- exec(state)
      tuple = newState match {
        case ts: TupleState => ts.execTuple.tuple
      }
    } yield tuple
    Tabulator.triplesToTable(tuples.toList)
  }
  
  
  private def paraphraseEval(input: String) = {
    val qState = QuestionState(input)
    val scores = { for {
      (absAction, absState) <- absArg(qState)
      (tempAction, tempState) <- templ(absState)
      step1 = QaStep(input, qState, absAction, absState)
      step2 = QaStep(input, absState, tempAction, tempState)
      score = -(cost(step1) + cost(step2))
    } yield (s"$absState -> $tempState" -> score) }.toMap
    val lines = for (key <- scores.keys.toList.sortBy(-scores(_))) yield s"$key\t${scores(key)}"
    lines.mkString("\n")
  }
  
  private def modeEval(input: String) = {
    if (evalFns.contains(input)) {
      evalFn = evalFns(input)
      s"Changing mode to $input"
    } else {
      throw new IllegalArgumentException(s"Unknown mode: $input")
    }
  }
  
  def result(d: Derivation) = {
    s"${d.answer}\n${d.score}\n$d\n${d.explainScore(model.costModel.weights)}\n"
  }
  def eval(input: String) = {
    if (input.startsWith("%")) {
      modeEval(input.slice(1, input.size))
    } else {
      evalFn(input)
    }
  }
}

object SimpleRepl extends App {
  
  override def main(args: Array[String]) = {
    import jline.console.ConsoleReader
    val reader = new ConsoleReader()
    val repl = new SimpleRepl
    while (true) {
	  val line = reader.readLine("> ")
	  try {
	    val result = repl.eval(line)
  	    println(result)
	  } catch {
	    case e: Throwable => {
	      println(s"Could not execute query: $e")
	      e.printStackTrace()
	    } 
	  }
    }
  }

}