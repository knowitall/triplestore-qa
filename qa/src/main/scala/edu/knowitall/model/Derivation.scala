package edu.knowitall.model

import edu.knowitall.search.qa.AnswerState
import edu.knowitall.search.qa.QuestionState
import edu.knowitall.search.Edge
import edu.knowitall.search.qa.QaState
import edu.knowitall.search.qa.QaAction
import edu.knowitall.learning.SparseVector
import edu.knowitall.search.qa.QaStep
import edu.knowitall.execution.ExecTuple
import edu.knowitall.search.qa.ExecutionAction
import edu.knowitall.execution.Tabulator

case class Derivation(question: String,
					  answer: String,
					  steps: IndexedSeq[QaStep],
					  features: SparseVector,
					  score: Double) {
  
  assert(steps.size >= 2)
  def questionState: QuestionState = steps.head.fromState match {
    case q: QuestionState => q
    case x =>
      throw new IllegalStateException(s"Expected QuestionState, got $x, steps = $steps")
  }
  def answerState: AnswerState = steps.last.toState match {
    case a: AnswerState => a
    case x =>
      throw new IllegalStateException(s"Expected AnswerState, got $x, steps = $steps")
  }
  def execTuple: ExecTuple = steps.last.action match {
    case a: ExecutionAction => a.execTuple
    case _ => 
      throw new IllegalStateException(s"Expected last action to be ExecutionAction. Found: ${steps.last.action}")
  }
  def explainScore(weights: SparseVector) = {
    val rows = for {
      fname <- features.activeComponents.toList.sortBy(f => -1*features(f)*weights(f))
      fvalue = features(fname)
      weight = weights(fname)
      product = fvalue * weight  
    } yield Seq(product, weight, fvalue, fname)
    
    val allRows: Seq[Seq[Any]] = Seq(Seq("prod", "weight", "value", "feature")) ++ rows.toSeq
    Tabulator.format(allRows)
  }
  override def toString = {
    val l = List(questionState) ++ steps.map(_.toState)
    l.map(_.toString).mkString(" -> ")
  }
  
}