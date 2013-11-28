package edu.knowitall.search.qa

import edu.knowitall.relsyn.RelSynClient
import edu.knowitall.search.Transition
import scalaz._
import Scalaz._
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.ListConjunctiveQuery
import org.slf4j.LoggerFactory

class RelSynTransition(client: RelSynClient = RelSynTransition.defaultClient) 
  extends Transition[QaState, QaAction] {
  
  val logger = LoggerFactory.getLogger(this.getClass)

  override def apply(s: QaState) = s match {
    case qs: QueryState if !qs.reformulated => reformulate(qs)
    case _ => Nil
  }
  
  private def reformulate(s: QueryState) = {
    val conjs = s.query.conjuncts
    for {
      i <- 0 until conjs.size
      c = conjs(i)
      rule <- client.relSyns(c)
      newc <- rule(c)
      newconjs = conjs.updated(i, newc)
      newq = ListConjunctiveQuery(s.query.qVars, newconjs)
      newstate = s.copy(query = newq, reformulated = true)
    } yield {
      logger.debug(s"new state => $newstate")
      (rule, newstate)
    }
  }

}

object RelSynTransition {
  lazy val defaultClient = new RelSynClient() 
}