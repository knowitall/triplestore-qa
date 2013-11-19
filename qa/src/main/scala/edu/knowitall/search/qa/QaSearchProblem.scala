package edu.knowitall.search.qa

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paraphrasing.template.TemplateParaphraser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.search.SearchProblem
import edu.knowitall.search.BeamSearch
import edu.knowitall.search.Transition
import edu.knowitall.search.Cost

case class QaSearchProblem(
    question: String,
    transitionModel: Transition[QaState, QaAction] = 
      QaSearchProblem.transitionModel,
    costModel: Cost[QaState, QaAction] = QaSearchProblem.costModel) 
    extends SearchProblem[QaState, QaAction] {

  val initialState = QuestionState(question)
  
  override def successors(s: QaState) = transitionModel(s)
    
  override def isGoal(s: QaState) = s match {
    case as: AnswerState => true
    case _ => false
  }
  
  override def cost(fromState: QaState, action: QaAction, toState: QaState) =
    costModel(fromState, action, toState)

}

object QaSearchProblem {
  
  val transitionModel = new Transition[QaState, QaAction] {
    override def apply(s: QaState): Iterable[(QaAction, QaState)] = Nil
  }
  
  val costModel = new Cost[QaState, QaAction] {
    override def apply(s1: QaState, a: QaAction, s2: QaState) = 0.0
  } 
  
}