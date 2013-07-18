package edu.knowitall.parsing

import edu.knowitall.execution.UQuery
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ListConjunctiveQuery

trait QuestionParser {
  
  def parse(q: String): Iterable[UQuery]

}

case class FormalQuestionParser() extends QuestionParser {
  override def parse(q: String) = ListConjunctiveQuery.fromString(q) match {
    case Some(cq: ListConjunctiveQuery) => ListConjunctiveQuery.expandSetTLiterals(cq)
    case _ => List()
  }
}