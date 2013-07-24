package edu.knowitall.scoring

import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.Feature.booleanToDouble
import edu.knowitall.tool.conf.FeatureSet
import scala.collection.immutable.SortedMap
import TupleFeatures._
import scala.language.implicitConversions

object AnswerGroupFeatures {

  type AnswerGroupFeature = Feature[AnswerGroup, Double]
  
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0
  
  def allTuples(group: AnswerGroup)  = group.derivations.map(_.etuple.tuple)
  def allQueries(group: AnswerGroup) = group.derivations.map(_.etuple.equery).distinct
  
  object NumberOfNamespaces extends AnswerGroupFeature("Number of distinct namespaces across all tuples") {
    def apply(group: AnswerGroup) = {
      val distinctNamespaces = allTuples(group).map(_.get("namespace").toString).distinct
      distinctNamespaces.size.toDouble
    }
  }
  
  object NumberOfAlternates extends AnswerGroupFeature("Number of alternate forms of the answer") {
    def apply(group: AnswerGroup) = group.alternates.size.toDouble
  }
  
  object NumberOfDerivations extends AnswerGroupFeature("Number of AnswerDerivations in the AnswerGroup") {
    def apply(group: AnswerGroup) = group.derivations.size.toDouble
  }
  
  object CapitalAnswerTokens extends AnswerGroupFeature("Number of Capitalized Answer Tokens") {
    val capitalRegex = "[A-Z]\\w+".r
    def apply(group: AnswerGroup) = group.alternates.map({ alternate =>
      alternate.map(answer => capitalRegex.findAllIn(answer).size).sum
    }).max
  }
  
  object CapitalQueryTokens extends AnswerGroupFeature("Number of Capitalized Query Tokens") {
    import CapitalAnswerTokens.capitalRegex
    def apply(group: AnswerGroup): Double = {
      val queryLiteralFields = allQueries(group).map {
        case equery: ExecConjunctiveQuery => equery.conjuncts.flatMap(_.literalFields)
        case _ => Nil
      }
      val queryLiterals = queryLiteralFields.map(_.map(_._2.toString))
      val queryLiteralCapitalCounts = queryLiterals.map(lits => lits.map(l => capitalRegex.findAllIn(l).size).sum)
      (0 :: queryLiteralCapitalCounts).max
    }
  }
  
  object CapitalTokenDisparity extends AnswerGroupFeature(
      "Absolute difference between number of capitalized tokens in answer and in query literals.") {
    def apply(group: AnswerGroup) = math.abs(CapitalAnswerTokens(group) - CapitalQueryTokens(group))
  }
  
  object AnswerStartsWithDeterminers extends AnswerGroupFeature("Answer starts with determiner") {
    val determiners = Set("the", "a", "an", "these", "those", "that", "this", "some", "most", "all", "any")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerToken = firstAnswer.split("\\s+").head.toLowerCase()
      determiners.contains(firstAnswerToken)
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
  
  /**
   * Generic features that apply to any AnswerGroup
   */
  val features: Seq[AnswerGroupFeature] = Seq(
      NumberOfNamespaces, 
      NumberOfAlternates,
      NumberOfDerivations,
      AnswerStartsWithDeterminers,
      AnswerContainsNegation)

  
  def featureSet: FeatureSet[AnswerGroup, Double] = FeatureSet(features)
}

object TupleFeatures {
  
  import edu.knowitall.execution.Tuple
  
  def isFromNamespace(ns: String)(tuple: Tuple) = tuple.get("namespace").exists {
    case s: String => s.contains(ns)
    case _ => false
  }
}