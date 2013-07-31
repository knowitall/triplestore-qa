package edu.knowitall.scoring.features

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup


object SystemFeatures {

  val postagConfig = QAConfig(parser = "formal", 
                              executor = "identity",
                              grouper = "postag",
                              scorer = "numDerivations")
  
  val postagSystem = QASystem.getInstance(postagConfig).getOrElse {
    throw new RuntimeException("Unable to load postag-answer system configuration.")
  }
  
  val postagGrouper = postagSystem.grouper
  
  object PostagRanking extends AnswerGroupFeature("Normalized postags ranking in query results.") {
    
    def apply(group: AnswerGroup) = {
      // get the queries run for this answergroup
      val queries = group.derivations.map(_.etuple.equery.uquery)
      val postagResults = postagSystem.answerUQueries(queries, queries.mkString(" | "))
      val postagRanking = postagResults.map(group => (group.answer, group.derivations.size)).toMap
      val groupPostags = postagGrouper.regroup(List(group)).map(_.answer).toSet
      val postagLookup = postagRanking.keys.find(postags => groupPostags.contains(postags))
      postagLookup match {
        case Some(postags) => postagRanking(postags)
        case None => 0
      }
    }    
  }
}