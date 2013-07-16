package edu.knowitall.triplestore

import edu.knowitall.triplestore.Search.TSQuery
import edu.knowitall.triplestore.Search.Field._
import edu.knowitall.triplestore.Search.FieldKeywords
import edu.knowitall.triplestore.Search.Conjunction
import scala.language.implicitConversions

trait UQuery

trait ExecQuery {
  val uquery: UQuery
}

case class ExecConjunctiveQuery(uquery: ConjunctiveQuery) extends ExecQuery {
  val conjuncts = uquery.conjuncts
  val projAttr = uquery.qAttr
}

case class ExecTuple(tuple: Tuple, equery: ExecQuery)

case class AnswerDerivation(attr: String, etuple: ExecTuple) {
  val answer = etuple.tuple.getString(attr) match {
    case Some(a) => a
    case _ => throw new 
      IllegalArgumentException(s"$etuple does not have val for attr $attr")
  }
}

trait QueryExecutor {
  def deriveAnswers(uquery: UQuery): Iterable[AnswerDerivation]
}

case class IdentityExecutor(client: TriplestoreClient) extends QueryExecutor {
  
  val joiner = Joiner(client)
  
  type ADs = Iterable[AnswerDerivation]
  
  def deriveAnswers(q: UQuery): ADs = q match {
    case c: ConjunctiveQuery => deriveAnswersSimple(ExecConjunctiveQuery(c))
    case _ => throw new 
      UnsupportedOperationException(s"Unable to execute query type: $q")
  }
  
  def deriveAnswersSimple(q: ExecConjunctiveQuery): ADs = { 
    val tuples = joiner.joinQueries(q.conjuncts)
    for (t <- tuples; et = ExecTuple(t, q)) 
      yield AnswerDerivation(q.projAttr, et)
  }

}