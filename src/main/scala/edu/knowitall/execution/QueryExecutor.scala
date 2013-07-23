package edu.knowitall.execution
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.rel
import edu.knowitall.triplestore.TriplestoreClient
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer

trait UQuery

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

case class RelationSynonymExecutor(client: TriplestoreClient, executor: QueryExecutor, max: Int = 5) extends QueryExecutor {
  val syns = TriplestoreRelationSynonyms(client, max)
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def expandConjunctiveQuery(q: ConjunctiveQuery): List[UQuery] = {
    val rels = q.conjuncts.flatMap(_.values.get(rel))
    logger.debug(s"rels = $rels")
    val litRels = rels.collect { case UnquotedTLiteral(v: String) => v }
    val rules = litRels.flatMap(syns.getRewrites(_)).distinct
    logger.debug(s"rules = $rules")
    val result = rules.map(r => r(q)).flatten :+ q
    logger.debug(s"result = $result")
    result.distinct
  }
  
  def expandQuery(q: UQuery): List[UQuery] = q match {
    case q: ConjunctiveQuery => {
      val results = expandConjunctiveQuery(q)
      logger.debug(s"Expanded query $q into $results")
      results
    }
    case _ => List(q)
  }
  
  def deriveAnswers(q: UQuery) = expandQuery(q).par.flatMap(executor.deriveAnswers(_)).toList
}