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

object MyTest extends App {
  val q = args(0)
  val qa = QASystem.getInstance().get
  val groups = qa.answer(q)
  for (g <- groups; d <- g.derivations) {
    val vec = AnswerDerivationFeatures.vectorize(d)
    val w1 = QueryTupleSimilarity.queryWords(d.parsedQuery, d.execTuple.tuple)
    val w2 = QueryTupleSimilarity.tupleWords(d.parsedQuery, d.execTuple.tuple)
    println(vec.map(x => "%.2f" format x).mkString("\t") + "\t" + d.answerString + "\t" + w1.mkString(" ") + "\t" + w2.mkString(" "))
  }
}