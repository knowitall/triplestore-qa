package edu.knowitall.search.qa

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paraphrasing.template.TemplateParaphraser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.search.SearchProblem
import edu.knowitall.search.BeamSearch
import edu.knowitall.search.Transition
import edu.knowitall.search.Cost
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.parsing.regex.RegexQuestionPatterns

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
  
  lazy val tagger = new StanfordPostagger
  lazy val stemmer = new MorphaStemmer
  lazy val tokenizer = new ClearTokenizer
  
  lazy val abstractArgs = new AbstractArgTransition(tagger = tagger,
      stemmer = stemmer, tokenizer = tokenizer)
  
  lazy val templates = new TemplateTransition
  
  lazy val chunker = new OpenNlpChunker
  lazy val parser = new RegexQuestionParser(chunker = chunker,
      postagger = tagger)
  lazy val parse = new RegexParseTransition(parser)
  
  val transitionModel = abstractArgs + templates + parse
  
  val costModel = new Cost[QaState, QaAction] {
    override def apply(s1: QaState, a: QaAction, s2: QaState) = 0.0
  } 
  
}

object MyTest extends App {
  
  val state0 = QuestionState("What is the capital city of France?")
  val f = QaSearchProblem.transitionModel
  
  
  for {
    (action1, state1) <- f(state0)
    (action2, state2) <- f(state1)
    (action3, state3) <- f(state2)
  } yield {
    println(state0)
    println(action1)
    println(state1)
    println(action2)
    println(state2)
    println(action3)
    println(state3)
    println
  }
  
  
}