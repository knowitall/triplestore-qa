package edu.knowitall.search

import edu.knowitall.search.qa.AnswerState
import edu.knowitall.search.qa.QueryState
import edu.knowitall.search.qa.QuestionState
import edu.knowitall.search.qa.QaState
import edu.knowitall.search.qa.QaAction

class QaSearchProblem(question: String) extends SearchProblem[QaState, QaAction] {
  
  type Path = SearchPath[QaState, QaAction]
  
  val initialState = QuestionState(question)
  
  override def successors(s: QaState) = s match {
    case qs: QuestionState => questionSuccessors(qs)
    case qs: QueryState => querySuccessors(qs)
    case as: AnswerState => answerSuccessors(as)
    case _ => Nil
  }
  
  def questionSuccessors(s: QuestionState) = null
  
  def querySuccessors(s: QueryState) = null
  
  def answerSuccessors(s: AnswerState) = null
  
  override def isGoal(s: QaState) = s match {
    case as: AnswerState => true
    case _ => false
  }
  
  override def cost(p:Path) = 0.0 

}