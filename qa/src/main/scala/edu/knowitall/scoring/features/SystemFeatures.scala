package edu.knowitall.scoring.features

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.UQuery
import com.twitter.util.LruMap
import org.slf4j.LoggerFactory

object SystemFeatures {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val postagConfig = QAConfig(parser = "formal",
                              executor = "identity",
                              grouper = "postag",
                              scorer = "numDerivations")

  val postagSystem = QASystem.getInstance(postagConfig).getOrElse {
    throw new RuntimeException("Unable to load postag-answer system configuration.")
  }

  val postagSystemCache = new LruMap[List[UQuery], List[AnswerGroup]](1000)

  def answerUQueriesCached(queries: List[UQuery]): List[AnswerGroup] = postagSystemCache.synchronized {
    val derivs = postagSystem.execute(queries)
    val groups = postagSystem.group(derivs)
    postagSystemCache.getOrElseUpdate(queries, groups.toList)
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
        case None => {
          logger.error("postag not found: " + postagLookup.map(_.toString))
          100.0 // throw new RuntimeException("postag not found.")
        }
      }
    }
  }

}