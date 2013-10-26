package edu.knowitall.scoring.features
import edu.knowitall.tool.conf.Feature
import scala.language.implicitConversions
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.tool.conf.FeatureSet
import scala.collection.immutable.SortedMap
import edu.knowitall.apps.QASystem
import edu.knowitall.paraphrasing.template.TemplateParaphraseDerivation
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory

abstract class AnswerDerivationFeature(name: String) extends Feature[AnswerDerivation, Double](name)

object AnswerDerivationFeatures extends FeatureSet[AnswerDerivation, Double] {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  val conf = ConfigFactory.load()
  val defaultPmi = conf.getDouble("paraphrase.defaultPmi")
  val defaultLm = conf.getDouble("paraphrase.defaultLm")
  
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
  
  object ParaphrasePmi extends AnswerDerivationFeature("paraphrase pmi") {
    def apply(d: AnswerDerivation) = d.paraphrase.derivation match {
      case pd: TemplateParaphraseDerivation => pd.pmi
      case _ => defaultPmi
    }
  }
  
  object ParaphraseLm extends AnswerDerivationFeature("paraphrase lm") {
    def apply(d: AnswerDerivation) = d.paraphrase.derivation match {
      case pd: TemplateParaphraseDerivation => pd.lm
      case _ => defaultLm
    }
  }
  
  object UsesJoin extends AnswerDerivationFeature("uses a join?") {
    def apply(d: AnswerDerivation) = d.execTuple.query.conjuncts.size > 1
  }
  
  object NumWords extends AnswerDerivationFeature("number of words in answer") {
    def apply(d: AnswerDerivation) = d.answerString.split(" ").size
  }
  
  private val features: Seq[AnswerDerivationFeature] = Seq(ParaphraseScore, IsParaphrased, Similarity, ParaphraseLm, ParaphrasePmi, UsesJoin, NumWords)
  override val featureMap =
    SortedMap.empty[String, Feature[AnswerDerivation, Double]] ++ features.map(feature => (feature.name, feature))
}

