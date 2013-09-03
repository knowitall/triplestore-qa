package edu.knowitall.scoring.features

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.UQuery
import com.twitter.util.LruMap

object SystemFeatures {

  val postagConfig = QAConfig(parser = "formal", 
                              executor = "identity",
                              grouper = "postag",
                              scorer = "numDerivations")
  
  val postagSystem = QASystem.getInstance(postagConfig).getOrElse {
    throw new RuntimeException("Unable to load postag-answer system configuration.")
  }
  
  val postagSystemCache = new LruMap[List[UQuery], List[AnswerGroup]](1000)
  
  def answerUQueriesCached(queries: List[UQuery]): List[AnswerGroup] = {
    postagSystemCache.getOrElseUpdate(queries, postagSystem.answerUQueries(queries, queries.mkString(" | ")))
  }
  
  val postagGrouper = postagSystem.grouper
  
  object PostagFrequency extends AnswerGroupFeature("Normalized postags frequency in query results.") {
    
    def apply(group: AnswerGroup) = {
      // get the queries run for this answergroup
      val queries = group.derivations.map(_.etuple.equery.uquery).distinct
      val postagResults = answerUQueriesCached(queries)
      val postagFrequencies = postagResults.map(group => (group.answer, group.derivations.size)).toMap
      val groupPostags = postagGrouper.regroup(List(group)).map(_.answer).toSet
      val postagLookup = postagFrequencies.keys.find(postags => groupPostags.contains(postags))
      postagLookup match {
        case Some(postags) => postagFrequencies(postags)
        case None => 0
      }
    }    
  }
  
  object PostagRanking extends AnswerGroupFeature("Normalized postags ranking in query results.") {
    
    def apply(group: AnswerGroup) = {
      // get the queries run for this answergroup
      val queries = group.derivations.map(_.etuple.equery.uquery).distinct
      val postagResults = answerUQueriesCached(queries)
      val postagFrequencies = postagResults.map(group => (group.answer, group.derivations.size)).toMap
      val sizeRanking = postagFrequencies.values.toSeq.distinct.sortBy(-_)
      val groupPostags = postagGrouper.regroup(List(group)).map(_.answer).toSet
      val postagLookup = postagFrequencies.keys.find(postags => groupPostags.contains(postags))
      postagLookup match {
        case Some(postags) => {
          val rank = sizeRanking.indexWhere(_ == postagFrequencies(postags))
          rank
        }
        case None => throw new RuntimeException("postag not found.")
      }
    }    
  }
  
}