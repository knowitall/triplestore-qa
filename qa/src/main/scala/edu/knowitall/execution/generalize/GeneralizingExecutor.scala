package edu.knowitall.execution
package generalize

import org.slf4j.LoggerFactory

abstract class GeneralizingExecutor extends QueryExecutor {

  type ADs = Iterable[AnswerDerivation]

  private val logger = LoggerFactory.getLogger(this.getClass)
  
  def baseExecutor: QueryExecutor 
  
  def generalizations(q: ListConjunctiveQuery): Iterator[ListConjunctiveQuery]
  
  def deriveAnswers(q: UQuery): ADs = q match {
    case c: ListConjunctiveQuery => { 
      val gens = generalizations(c).toSeq
      gens.map(_.toString) foreach logger.info
      val queryResponses = (Seq(c) ++ gens) map baseExecutor.deriveAnswers
      queryResponses.find(_.nonEmpty).getOrElse(Nil)
    }
    case _ => throw new 
      UnsupportedOperationException(s"Unable to execute query type: $q")
  } 
}