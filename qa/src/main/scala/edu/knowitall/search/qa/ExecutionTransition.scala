package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.Joiner
import edu.knowitall.execution.ExecTuple
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.execution.IdentityExecutor
import com.typesafe.config.ConfigFactory
import edu.knowitall.execution.ConjunctiveQuery
import org.apache.solr.client.solrj.SolrServerException
import java.io.StringWriter
import java.io.PrintWriter
import org.slf4j.LoggerFactory

class ExecutionTransition(
    client: TriplestoreClient = ExecutionTransition.defaultClient,
    skipTimeouts: Boolean = ExecutionTransition.defaultSkipTimeouts) 
    extends Transition[QaState, QaAction] {
  
  private val logger = LoggerFactory.getLogger(this.getClass) 
  
  private val executor = DefaultFilters.wrap(IdentityExecutor(client))
  
  private final val action = ExecutionAction()
  
  override def apply(s: QaState) = s match {
    case s: QueryState => executeQuery(s) 
    case _ => Nil
  }
  
  private def executeQuery(state: QueryState) = for {
    etuple <- execute(state.query)
    newState = AnswerState(etuple.answerString, etuple)
  } yield (action, newState)
  
  private def execute(query: ConjunctiveQuery) = try {
    executor.execute(query)
  } catch {
    case e: SolrServerException => if (skipTimeouts) {
      val sw = new StringWriter()
      val pw = new PrintWriter(sw)
      e.printStackTrace(pw)
      logger.warn(s"Could not execute query: $query, got ${sw.toString}")
      List.empty
    } else {
      throw e
    }
  }

}

case class ExecutionAction() extends QaAction

object ExecutionTransition {
  val conf = ConfigFactory.load()
  val defaultSkipTimeouts = conf.getBoolean("triplestore.skipTimeouts")
  lazy val defaultClient = new SolrClient() 
}