package edu.knowitall.search.qa

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paraphrasing.template.TemplateParaphraser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.search.SearchProblem
import edu.knowitall.search.BeamSearch

class QaSearchProblem(question: String) extends SearchProblem[QaState, QaAction] {

  val initialState = QuestionState(question)
  
  val parser = new RegexQuestionParser()
  val paraphraser = new TemplateParaphraser()
  val executor = new IdentityExecutor(new SolrClient())
  
  case class DummyAction(name: String) extends QaAction
  val ParseAction = DummyAction("parsed")
  val ParaphraseAction = DummyAction("paraphrased")
  val ExecuteAction = DummyAction("executed")
  
  override def successors(s: QaState) = s match {
    case qs: QuestionState => questionSuccessors(qs)
    case qs: QueryState => querySuccessors(qs)
    case _ => Nil
  }
  
  def questionSuccessors(s: QuestionState) = {
    val q = s.question
    val pps = for (pp <- paraphraser.paraphrase(q); ppst = pp.target) yield (ParaphraseAction, QuestionState(ppst))
    val qs = for (q <- parser.parse(q)) yield (ParseAction, QueryState(q))
    pps ++ qs
  }
  
  def querySuccessors(s: QueryState) = {
    val query = s.query
    val hits = executor.execute(query)
    for (hit <- hits) yield (ExecuteAction, AnswerState(hit.answerString, hit))
  }
  
  override def isGoal(s: QaState) = s match {
    case as: AnswerState => true
    case _ => false
  }
  
  override def cost(from: QaState, action: QaAction, toState: QaState) = {
    if (action == ParaphraseAction) 2.0
    else 1.0
  }

}

object QaSearchProblem extends App {
  val q = args(0)
  val problem = new QaSearchProblem(q)
  val searcher = new BeamSearch(problem, 5, 10, 1)
  val goals = searcher.search
  goals map { g => g.state } collect { case AnswerState(a, t) => println(a) }
}