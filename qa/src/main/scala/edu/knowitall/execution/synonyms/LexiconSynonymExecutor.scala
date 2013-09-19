package edu.knowitall.execution.synonyms

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.execution.Search
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.Utils

class LexiconSynonymExecutor(val synServer: LexiconSynonyms, val baseExecutor: QueryExecutor) extends QueryExecutor {
  
  def this(baseExecutor: QueryExecutor) = this(new LexiconSynonyms(), baseExecutor)
  
  def expandConjunct(c: TConjunct): List[TConjunct] = {
    val queryArg1 = c.values.get(Search.arg1).collect { case UnquotedTLiteral(v: String) => v.toLowerCase }
    val queryArg2 = c.values.get(Search.arg2).collect { case UnquotedTLiteral(v: String) => v.toLowerCase }
    val queryArg1Syns = queryArg1.toList.flatMap(s => s +: synServer.entSynonymsFor(s))
    val queryArg2Syns = queryArg2.toList.flatMap(s => s +: synServer.entSynonymsFor(s))
    
    (queryArg1, queryArg2) match {
      case (None, None) => List(c)
      case (Some(_), None) => queryArg1Syns.map(qs => c.copy(values = c.values + (Search.arg1 -> QuotedTLiteral(qs))))
      case (None, Some(_)) => queryArg2Syns.map(qs => c.copy(values = c.values + (Search.arg2 -> QuotedTLiteral(qs))))
      case (Some(_), Some(_)) => {
        val oldValues = c.values
        val arg1Expanded = queryArg1Syns.map(qs => c.copy(values = c.values + (Search.arg1 -> QuotedTLiteral(qs))))
        val fullExpanded = arg1Expanded.flatMap { a1exp =>
          queryArg2Syns.map(qs => a1exp.copy(values = a1exp.values + (Search.arg2 -> QuotedTLiteral(qs))))
        }
        fullExpanded
      }
    }
  }
   
  def expandConjQuery(q: ConjunctiveQuery): List[ConjunctiveQuery] = {
    val newCs = q.conjuncts.map(expandConjunct).distinct
    val conjSets = Utils.cartesian[TConjunct](newCs).toList
    conjSets.map(cs => ListConjunctiveQuery(q.qVars, cs.toList))
  }
  
  def expandQuery(uquery: UQuery): List[UQuery] = uquery match {
    case q: ConjunctiveQuery => expandConjQuery(q)
    case _ => List(uquery)
  }
  
  def deriveAnswers(uquery: UQuery): Iterable[AnswerDerivation] = expandQuery(uquery).flatMap(baseExecutor.deriveAnswers)
}
