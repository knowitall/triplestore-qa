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
  
}