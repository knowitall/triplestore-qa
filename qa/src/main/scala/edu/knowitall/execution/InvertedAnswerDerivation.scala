package edu.knowitall.execution

import edu.knowitall.paraphrasing.Paraphrase
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.apps.AnswerDerivation

case class InvertedAnswerDerivation(execQuery: ConjunctiveQuery, 
							  		tables: List[ConjunctTable],
							  		parsedQueries: List[ConjunctiveQuery],
							  		paraphrases: List[Paraphrase])
							   
case object InvertedAnswerDerivation {
  def fromDerivations(derivs: List[AnswerDerivation]): List[InvertedAnswerDerivation] = {
    val groupedDerivs = derivs.groupBy(_.execTuple.query)
    val invGroups = for ((execQuery, derivGroup) <- groupedDerivs;
    					 execTuples = derivGroup.map(_.execTuple);
    					 tables = ConjunctTable.factorExecTuples(execTuples);
    					 parsedQueries = derivGroup.map(_.parsedQuery).distinct;
    					 paraphrases = derivGroup.map(_.paraphrase).distinct) 
      yield InvertedAnswerDerivation(execQuery, tables, parsedQueries, paraphrases)
    invGroups.toList
  }
}