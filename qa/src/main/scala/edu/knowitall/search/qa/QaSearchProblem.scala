package edu.knowitall.search.qa

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paraphrasing.template.TemplateParaphraser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.search.SearchProblem
import edu.knowitall.search.BeamSearch
import edu.knowitall.search.Transition
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.parsing.regex.RegexQuestionPatterns
import edu.knowitall.triplestore.CachedTriplestoreClient

case class QaSearchProblem(
    question: String,
    transitionModel: Transition[QaState, QaAction] = 
      QaSearchProblem.transitionModel,
    costModel: Function[QaStep, Double] = QaSearchProblem.costModel) 
    extends SearchProblem[QaState, QaAction] {

  val initialState = QuestionState(question)
  
  override def successors(s: QaState) = transitionModel(s)
    
  override def isGoal(s: QaState) = s match {
    case as: AnswerState => true
    case _ => false
  }
  
  override def cost(fromState: QaState, action: QaAction, toState: QaState) =
    costModel(QaStep(question, fromState, action, toState))

}

object QaSearchProblem {
  
  val transitionModel = new QaTransitionModel
  
  val costModel = new QaCostModel
  
}

object MyTest extends App {
  
  val question = "What is the capital city of France?"
  val state0 = QuestionState(question)
  val trans = QaSearchProblem.transitionModel
  val cost = new QaCostModel
  val f = QaFeatures
  
  for {
    (action1, state1) <- trans(state0)
    feat1 = f(QaStep(question, state0, action1, state1))
    (action2, state2) <- trans(state1)
    feat2 = f(QaStep(question, state1, action2, state2))
    (action3, state3) <- trans(state2)
    feat3 = f(QaStep(question, state2, action3, state3))
    (action4, state4) <- trans(state3)
    feat4 = f(QaStep(question, state3, action4, state4))
  } yield {
    println(state0)
    println(action1)
    println(state1)
    println(action2)
    println(state2)
    println(action3)
    println(state3)
    println(action4)
    println(state4)
    println((feat1 + feat2 + feat3 + feat4))
    println
  }
  
  
}