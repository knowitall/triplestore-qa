package edu.knowitall.scoring

import edu.knowitall.learning.FeatureFunction
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.learning.SparseVector
import edu.knowitall.execution.AnswerGroup
import com.typesafe.config.ConfigFactory


class AnswerDerivationScorer(features: FeatureFunction[AnswerDerivation] = AnswerDerivationScorer.defaultFeatures,
    weights: SparseVector = AnswerDerivationScorer.defaultWeights) extends AnswerScorer {
  
  def scoreDerivation(d: AnswerDerivation): Double = features(d) * weights
  
  def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup = {
    val score = group.derivations.map(scoreDerivation).max
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, score)
  }

}

object AnswerDerivationScorer {
  val conf = ConfigFactory.load()
  val weightsPath = conf.getString("scoring.weights")
  val defaultWeights = SparseVector.fromFile(weightsPath)
  val defaultFeatures = null
}