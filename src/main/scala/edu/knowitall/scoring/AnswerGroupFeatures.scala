package edu.knowitall.scoring

import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.Feature.booleanToDouble
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.common.Resource.using
import scala.collection.immutable.SortedMap
import TupleFeatures._
import scala.language.implicitConversions
import io.Source

object AnswerGroupFeatures {

  type AnswerGroupFeature = Feature[AnswerGroup, Double]
  
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0
  
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 1000)
  
  
  def allTuples(group: AnswerGroup)  = group.derivations.map(_.etuple.tuple)
  def allQueries(group: AnswerGroup) = group.derivations.map(_.etuple.equery).distinct
  
  object MultipleNamespaces extends AnswerGroupFeature("Answer comes from multiple namespaces") {
    def apply(group: AnswerGroup) = {
      val distinctNamespaces = allTuples(group).map(_.get("namespace").toString).distinct
      distinctNamespaces.size > 1
    }
  }
  
  object NumberOfAlternates extends AnswerGroupFeature("Number of alternate forms of the answer") {
    def apply(group: AnswerGroup) = group.alternates.size.toDouble
  }
  
  object NumberOfDerivations extends AnswerGroupFeature("Number of AnswerDerivations in the AnswerGroup") {
    def apply(group: AnswerGroup) = group.derivations.size.toDouble
  }
  
  object AnswerContainsArticles extends AnswerGroupFeature("Answer contains article tokens") {
    val articles = Set("the", "a", "an")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      articles.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerStartsWithDeterminers extends AnswerGroupFeature("Answer contains determiner") {
    val determiners = Set("these", "those", "that", "this", "some", "most", "all", "any", "both", "either")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      determiners.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerContainsNegation extends AnswerGroupFeature("Answer contains a negation word") {
    val negationWords = Set("no", "none", "never", "neither", "nobody", "nor", "nothing", "nowhere", "n't")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      negationWords.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object TriplestoreAnswerFrequency extends AnswerGroupFeature("Log of Frequency of answer in the triplestore") {
    val junkTokens = Set("a", "an", "the", "or", "and", "&") ++ AnswerStartsWithDeterminers.determiners
    val splitRegex = "\\s+".r
    def apply(group: AnswerGroup) = {
      val rawAnswer = group.alternates.head.head.toLowerCase
      val answerTokens = splitRegex.split(rawAnswer)
      val filteredTokens = answerTokens.filter(tok => !junkTokens.contains(tok)).toSeq
      val escapedTokens = filteredTokens map SolrClient.escape
      val cleanAnswer = if (escapedTokens.nonEmpty) escapedTokens.mkString("\"", " ", "\"") else Seq("*")
      val query = new TSQuery { 
        val toQueryString = "arg1:%s".format(cleanAnswer) 
        override val toString = toQueryString
      }
      math.log(client.count(query).toDouble + 1)
    }
  }
  
  /**
   * Generic features that apply to any AnswerGroup
   */
  val features: Seq[AnswerGroupFeature] = Seq(
      MultipleNamespaces, 
      NumberOfAlternates,
      NumberOfDerivations,
      AnswerContainsArticles,
      AnswerStartsWithDeterminers,
      AnswerContainsNegation,
      TriplestoreAnswerFrequency)

  
  def featureSet: FeatureSet[AnswerGroup, Double] = FeatureSet(features)
}

object TupleFeatures {
  
  import edu.knowitall.execution.Tuple
  
  def isFromNamespace(ns: String)(tuple: Tuple) = tuple.get("namespace").exists {
    case s: String => s.contains(ns)
    case _ => false
  }
}