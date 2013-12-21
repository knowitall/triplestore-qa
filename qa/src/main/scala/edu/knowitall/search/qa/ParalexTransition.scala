package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.paralex.ParalexClient
import com.typesafe.config.ConfigFactory

class ParalexTransition(client: ParalexClient = ParalexTransition.defaultClient) extends Transition[QaState, QaAction] {
  override def apply(s: QaState) = s match {
    case qs: QuestionState => parse(qs)
    case _ => Nil
  }
  def parse(qs: QuestionState) = {
    val q = qs.question
    for {
      record <- client.parse(q)
      query = record.query
      queryState = QueryState(query)
    } yield (record, queryState)
  }
}

object ParalexTransition {
  val conf = ConfigFactory.load()
  lazy val defaultClient = ParalexClient()
}