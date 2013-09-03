package edu.knowitall.execution

import synonyms.LexiconSynonyms

class LexiconSynonymExecutor(val synServer: LexiconSynonyms, val baseExecutor: QueryExecutor) extends QueryExecutor {
  
  def this(baseExecutor: QueryExecutor) = this(new LexiconSynonyms(), baseExecutor)
  
  def expandConjunct(c: TConjunct): List[TConjunct] = {
    val queryArg1 = c.values.get(Search.arg1).collect { case UnquotedTLiteral(v: String) => v.toLowerCase }
    val queryArg2 = c.values.get(Search.arg2).collect { case UnquotedTLiteral(v: String) => v.toLowerCase }
    val queryArg1Syns = queryArg1.toList.flatMap(s => s +: synServer.entSynonymsFor(s))
    val queryArg2Syns = queryArg2.toList.flatMap(s => s +: synServer.entSynonymsFor(s))
    val queryArg1TVals = SetTLiteral(queryArg1Syns.map(QuotedTLiteral(_)))
    val queryArg2TVals = SetTLiteral(queryArg2Syns.map(QuotedTLiteral(_)))
    
    (queryArg1, queryArg2) match {
      case (None, None) => List(c)
      case (Some(_), None) => List(c.copy(values = c.values + (Search.arg1 -> queryArg1TVals)))
      case (None, Some(_)) => List(c.copy(values = c.values + (Search.arg2 -> queryArg2TVals)))
      case (Some(_), Some(_)) => {
        val oldValues = c.values
        val arg1Expanded = c.copy(values = c.values + (Search.arg1 -> queryArg1TVals))
        val fullExpanded = arg1Expanded.copy(values = c.values + (Search.arg2 -> queryArg2TVals))
        List(fullExpanded)
      }
    }
  }
   
  def expandConjQuery(q: ConjunctiveQuery): List[ConjunctiveQuery] = {
    val newCs = q.conjuncts.map(expandConjunct)
    val conjSets = Utils.cartesian[TConjunct](newCs).toList
    conjSets.map(cs => ListConjunctiveQuery(q.qVars, cs.toList))
  }
  
  def expandQuery(uquery: UQuery): List[UQuery] = uquery match {
    case q: ConjunctiveQuery => expandConjQuery(q)
    case _ => List(uquery)
  }
  
  def deriveAnswers(uquery: UQuery): Iterable[AnswerDerivation] = expandQuery(uquery).flatMap(baseExecutor.deriveAnswers)
}
