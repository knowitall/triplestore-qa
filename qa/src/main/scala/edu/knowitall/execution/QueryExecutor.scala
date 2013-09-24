package edu.knowitall.execution
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.rel
import edu.knowitall.triplestore.TriplestoreClient
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.execution.synonyms.TriplestoreRelationSynonyms

trait UQuery {
  val question: String
}

trait ExecQuery {
  val uquery: UQuery
}

case class ExecConjunctiveQuery(uquery: ConjunctiveQuery, query: ConjunctiveQuery) extends ExecQuery {
  val conjuncts = uquery.conjuncts
  val projAttrs = uquery.qAttrs
}

case class ExecTuple(tuple: Tuple, equery: ExecQuery)

case class AnswerDerivation(attrs: List[String], etuple: ExecTuple) {
  val candAnswers = attrs.map(etuple.tuple.getString(_))
  val answer = candAnswers.map(x => x match {
    case Some(s: String) => s
    case None => throw new IllegalArgumentException(s"$etuple does not have vals for attrs $attrs")
  })
}

trait QueryExecutor {
  def deriveAnswers(uquery: UQuery): Iterable[AnswerDerivation]
}

case class IdentityExecutor(client: TriplestoreClient) extends QueryExecutor {
  
  val logger = LoggerFactory.getLogger(this.getClass) 
  
  val joiner = Joiner(client)
  
  type ADs = Iterable[AnswerDerivation]
  
  def deriveAnswers(q: UQuery): ADs = q match {
    case c: ConjunctiveQuery => deriveAnswersSimple(ExecConjunctiveQuery(c, c))
    case _ => throw new 
      UnsupportedOperationException(s"Unable to execute query type: $q")
  }
  
  def deriveAnswersSimple(q: ExecConjunctiveQuery): ADs =
    for (t <- joiner.joinQueries(q.query.conjuncts);
         et = ExecTuple(t, q)) 
      yield AnswerDerivation(q.projAttrs, et)

}