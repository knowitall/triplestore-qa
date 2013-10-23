package edu.knowitall.scoring.features
import edu.knowitall.tool.conf.Feature
import scala.language.implicitConversions
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.tool.conf.FeatureSet
import scala.collection.immutable.SortedMap
import edu.knowitall.apps.QASystem

abstract class AnswerDerivationFeature(name: String) extends Feature[AnswerDerivation, Double](name)

object AnswerDerivationFeatures extends FeatureSet[AnswerDerivation, Double] {
  
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0
  
  object ParaphraseScore extends AnswerDerivationFeature("paraphrase score") {
    def apply(d: AnswerDerivation) = d.paraphrase.derivation.score 
  }
  
  object IsParaphrased extends AnswerDerivationFeature("is paraphrased?") {
    def apply(d: AnswerDerivation) = (d.paraphrase.source != d.paraphrase.target)
  }
  
  object Similarity extends AnswerDerivationFeature("similarity between parsed query and tuple") {
    def apply(d: AnswerDerivation): Double = QueryTupleSimilarity.similarity(d.parsedQuery, d.execTuple.tuple) 
  }
  
  private val features: Seq[AnswerDerivationFeature] = Seq(ParaphraseScore, IsParaphrased, Similarity)
  override val featureMap =
    SortedMap.empty[String, Feature[AnswerDerivation, Double]] ++ features.map(feature => (feature.name, feature))
}

