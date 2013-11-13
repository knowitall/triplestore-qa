package edu.knowitall.scoring.learning

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.apps.AnswerGenerator
import edu.knowitall.apps.QASystem
import edu.knowitall.eval.Oracle
import edu.knowitall.scoring.features.AnswerDerivationFeatures
import org.slf4j.LoggerFactory
import edu.knowitall.execution.StrSim

case class QAModel(generator: AnswerGenerator, features: Function[AnswerDerivation, SparseVector] = QAFeatures, weights: SparseVector = SparseVector()) extends HiddenVariableModel[String, AnswerDerivation] {
  
  val logger = LoggerFactory.getLogger(this.getClass)

  type Deriv = AnswerDerivation
  
  def scoreDerivation(d: Deriv) = features(d) * weights
  
  def score(q: String, d: Deriv) = scoreDerivation(d)
  
  override def update(q: String, predicted: Deriv, expected: Deriv) = {
    val delta = features(expected) - features(predicted)
    val newWeights = weights + delta
    logger.debug(s"Updating for ($q, ${predicted.answerString}, ${expected.answerString})")
    logger.debug(s"Update delta = $delta")
    logger.debug(s"Old weights =\n$weights")
    logger.debug(s"Updated weights =\n$newWeights")
    this.copy(weights = newWeights)
  }
    
  override def sum(that: Model) = this.copy(weights = weights + that.weights)
  
  override def scale(c: Double) = this.copy(weights = weights * c)
  
  override def predict(q: String) = generator(q).toList match {
    case derivs if derivs.size > 0 => {
      val d = derivs.maxBy(scoreDerivation)
      logger.debug(s"Prediction: $q => ${d.answerString}")
      Some(d)
    }
    case _ => {
      logger.debug(s"Could not predict for $q")
      None
    }
  }
  
  override def candidatePredictions(q: String) = generator(q).toSeq
  
  def normalizeAnswer(answer: String): String = StrSim.norm(Oracle.normalize(answer))


}