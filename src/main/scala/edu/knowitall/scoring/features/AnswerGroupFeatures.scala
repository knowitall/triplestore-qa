package edu.knowitall.scoring.features
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.CountCachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecQuery
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution.Tuple
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import TupleFeatures._
import edu.knowitall.execution.Search.TSField
import edu.knowitall.execution.Search.rel
import edu.knowitall.execution.TConjunct
import edu.knowitall.tool.stem.MorphaStemmer
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

abstract class AnswerGroupFeature(name: String) extends Feature[AnswerGroup, Double](name)

object AnswerGroupFeatures extends FeatureSet[AnswerGroup, Double] {
  
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0
  
  def allTuples(group: AnswerGroup)  = group.derivations.map(_.etuple.tuple)
  def allQueries(group: AnswerGroup) = group.derivations.map(_.etuple.equery).distinct
  
  object MultipleNamespaces extends AnswerGroupFeature("Answer comes from multiple namespaces") {
    def apply(group: AnswerGroup) = {
      def getNamespace(t: Tuple) = t.get("namespace").toString
      val tupleNamespaces = allTuples(group) map getNamespace
      val distinctNamespaces = tupleNamespaces.distinct
      distinctNamespaces.size.toDouble
    }
  }
  
  
  
  object NumberOfAlternates extends AnswerGroupFeature("Number of alternate forms of the answer") {
    def apply(group: AnswerGroup) = group.alternates.size.toDouble
  }
  
  object NumberOfDerivations extends AnswerGroupFeature("Number of AnswerDerivations in the AnswerGroup") {
    def apply(group: AnswerGroup) = {
      val numDerivations = group.derivations.size.toDouble
      math.sqrt(numDerivations)
    }
  }
  
  object AnswerContainsArticles extends AnswerGroupFeature("Answer contains article tokens") {
    val articles = Set("the", "a", "an")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      articles.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerContainsDeterminer extends AnswerGroupFeature("Answer contains determiner") {
    val determiners = Set("these", "those", "that", "this", "some", "most", "all", "any", "both", "either", "each", "more", "less")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      determiners.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerContainsNegation extends AnswerGroupFeature("Answer contains a negation word") {
    val negationWords = Set("no", "none", "never", "neither", "nobody", "nor", "nothing", "nowhere", "n't", "cannot", "cant", "can't", "wont")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      negationWords.intersect(firstAnswerTokens).nonEmpty
    }
  }
  

  
  /**
   * Generic features that apply to any AnswerGroup
   */
  private val features: Seq[AnswerGroupFeature] = Seq(
      NumberOfDerivations,
      AnswerContainsArticles,
      AnswerContainsDeterminer,
      AnswerContainsNegation,
      LiteralFieldsDifference,
      TriplestoreFeatures.AnswerFrequency,
      SystemFeatures.PostagRanking
    )

  
  override val featureMap = 
    SortedMap.empty[String, Feature[AnswerGroup, Double]] ++ features.map(feature => (feature.name, feature))
}

object TupleFeatures {
  
  import edu.knowitall.execution.Tuple
  
  def isFromNamespace(ns: String)(tuple: Tuple) = tuple.get("namespace").exists {
    case s: String => s.contains(ns)
    case _ => false
  }
}