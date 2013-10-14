package edu.knowitall.execution
package generalize

import org.slf4j.LoggerFactory

abstract class GeneralizingExecutor extends QueryExecutor {

  private val logger = LoggerFactory.getLogger(this.getClass)
  
  def baseExecutor: QueryExecutor 
  
  def generalizations(q: ConjunctiveQuery): Iterator[ConjunctiveQuery]
  
  override def execute(q: ConjunctiveQuery): Iterable[ExecTuple] = {
    val gens = Iterator(q) ++ generalizations(q)
    val queryResponses = gens map baseExecutor.execute
    queryResponses.find(_.nonEmpty).getOrElse(Nil)
  }

}