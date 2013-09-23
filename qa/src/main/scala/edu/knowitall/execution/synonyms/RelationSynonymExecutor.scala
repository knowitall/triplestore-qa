package edu.knowitall.execution
package synonyms

import edu.knowitall.triplestore.TriplestoreClient
import org.slf4j.LoggerFactory
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.rel

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