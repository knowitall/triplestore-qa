package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.Joiner
import edu.knowitall.execution.ExecTuple
import edu.knowitall.triplestore.SolrClient

class ExecutionTransition(
    client: TriplestoreClient = ExecutionTransition.defaultClient) 
    extends Transition[QaState, QaAction] {
  
  private val joiner = Joiner(client)
  
  private final val action = ExecutionAction()
  
  override def apply(s: QaState) = s match {
    case s: QueryState => executeQuery(s) 
    case _ => Nil
  }
  
  private def executeQuery(state: QueryState) = for {
    tuple <- joiner.joinQueries(state.query.conjuncts)
    etuple = ExecTuple(tuple, state.query)
    newState = AnswerState(etuple.answerString, etuple)
  } yield (action, newState)

}

case class ExecutionAction() extends QaAction

object ExecutionTransition {
  lazy val defaultClient = new SolrClient() 
}