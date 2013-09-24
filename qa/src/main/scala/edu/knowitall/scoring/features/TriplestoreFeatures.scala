package edu.knowitall.scoring.features

import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.ExecQuery
import edu.knowitall.execution.Search.CountQuery
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.triplestore.CachedTriplestoreClient

object TriplestoreFeatures {
  
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:10893/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 10000)
  
  object QueryFrequency extends AnswerGroupFeature("Log of query literals in the triplestore") {
    
    import AnswerGroupFeatures.AnswerContainsDeterminer.determiners
    
    val junkTokens = Set("a", "an", "the", "or", "and", "&") ++ determiners
    val splitRegex = "\\s+".r
    
    def apply(group: AnswerGroup) = {
      val queries = group.derivations.map(_.etuple.equery).distinct
      def conjunctCounts(eq: ExecQuery) = eq match {
        case q: ExecConjunctiveQuery => {
          val literalFields = q.conjuncts.flatMap(_.literalFields)
          val counts = literalFields.map { case (field, value) =>
            client.count(value.toConjunct(field))  
          }
          counts
        }
        case _ => throw new RuntimeException("unknown query type.")
      }
      
      val counts = queries flatMap conjunctCounts
      
      val avg = counts.sum.toDouble / counts.size.toDouble
      
      math.log(avg + 1)
    }
  }

  object AnswerFrequency extends AnswerGroupFeature("Log of Frequency of answer in the triplestore") {
    
    import AnswerGroupFeatures.AnswerContainsDeterminer.determiners
    import QueryFrequency.splitRegex
    val junkTokens = Set("a", "an", "the", "or", "and", "&") ++ determiners
    

    
    def apply(group: AnswerGroup) = {
      
      val rawAnswer = group.alternates.head.head.toLowerCase
      val answerTokens = splitRegex.split(rawAnswer)
      val filteredTokens = answerTokens.filter(tok => !junkTokens.contains(tok)).toSeq
      val escapedTokens = filteredTokens map SolrClient.escape
      val cleanAnswer = if (escapedTokens.nonEmpty) escapedTokens.mkString(" ") else "" 
      val query = CountQuery(cleanAnswer)
      math.log(client.count(query).toDouble + 1)
    }
  }
}