package edu.knowitall.scoring.features

import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecQuery
import edu.knowitall.execution.ExecTuple
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

    /**
   * Generic features that apply to any AnswerGroup
   */
  private val features: Seq[AnswerGroupFeature] = Seq(
      MinConfidence,
      NumberOfDerivations,
      AnswerContainsNegation,
      LiteralFieldsDifference,
      MatchesPostagPatterns(Set("DT NN", "DT NNS", "DT JJ", "DT VGB"))
    )

//        /**
//   * Generic features that apply to any AnswerGroup
//   */
//  private val features: Seq[AnswerGroupFeature] = Seq(
//      NumberOfDerivations,
//      AnswerContainsArticles,
//      AnswerContainsDeterminer,
//      AnswerContainsNegation,
//      LiteralFieldsDifference,
//      TriplestoreFeatures.AnswerFrequency,
//      SystemFeatures.PostagRanking
//    )

  object QuestionQueryOverlap extends AnswerGroupFeature("Question / Query overlap") {
    def apply(group: AnswerGroup) = {
      def getNamespace(t: Tuple) = t.get("namespace").toString
      val tupleNamespaces = allTuples(group) map getNamespace
      val distinctNamespaces = tupleNamespaces.distinct
      distinctNamespaces.size.toDouble
    }
  }

  object MinConfidence extends AnswerGroupFeature("Minimum Confidence") {
    val confRegex = ".*conf.*".r
    val min = 0.05
    def apply(group: AnswerGroup) = {
      val tuples = group.derivations.map(_.etuple.tuple)
      val allAttrs = tuples.flatMap(_.attrs.iterator.toSeq)
      val confs = allAttrs.collect {
        case (confRegex(), x: Float) => math.max(min, x.toDouble)
      }
      if (confs.isEmpty) min else confs.min
    }
  }

  // TODO: add to featureset after finishing new training data 9-26...
  case class MatchesPostagPatterns(patterns: Set[String]) extends AnswerGroupFeature("Answer postags in " + patterns.toString) {
    import edu.knowitall.execution.PostagAnswerGrouper.normalize
    def apply(group: AnswerGroup) = {
      val postags = normalize(group.alternates.head.head)
      patterns.contains(postags)
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

  object NumberUniqueTriples extends AnswerGroupFeature("Number of Unique Triples in the AnswerGroup") {

    val requiredFieldNames = Seq("arg1", "rel", "arg2", "namespace")

    def apply(group: AnswerGroup) = {
      val etuples = group.derivations.map(_.etuple)
      val tupleQueries = etuples.collect { case ExecTuple(tuple, ExecConjunctiveQuery(_, query)) => (tuple, query) }
      val triples = tupleQueries.flatMap { case (tuple, query) =>
        // get a map of conjunct name -> conjunct attrs
        val conjName = query.conjuncts.map(_.name)
        // extract the triple for each conjunct
        conjName.map { prefix =>
          val attrs = requiredFieldNames.map(f => prefix+"."+f)
          attrs.flatMap(tuple.getString(_))
        }
      }
      val distinctTriples = triples.distinct
      triples.distinct.size
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