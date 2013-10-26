package edu.knowitall.scoring.learning

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.apps.AnswerGenerator
import edu.knowitall.apps.QASystem
import edu.knowitall.eval.Oracle
import edu.knowitall.scoring.features.AnswerDerivationFeatures
import org.slf4j.LoggerFactory

case class QAModel(generator: AnswerGenerator, features: Function[AnswerDerivation, SparseVector], weights: SparseVector) extends HiddenVariableModel[String, AnswerDerivation, Set[String]] {
  
  val logger = LoggerFactory.getLogger(this.getClass)

  type Deriv = AnswerDerivation
  
  def scoreDerivation(d: Deriv) = features(d) * weights
  
  override def score(q: String, d: Deriv) = scoreDerivation(d)
  
  override def update(q: String, predicted: Deriv, expected: Deriv) = {
    val delta = features(expected) - features(predicted)
    val newWeights = weights + delta
    logger.debug(s"Updating for ($q, ${predicted.answerString}, ${expected.answerString})")
    logger.debug(s"Update delta = $delta")
    logger.debug(s"Updated weights = $newWeights")
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
  
  override def predictConstrained(q: String, expected: Set[String]) = generator(q).filter(deriv => isCorrect(q, project(deriv), expected)) match {
    case derivs if derivs.size > 0 => {
      val d = derivs.maxBy(scoreDerivation)
      logger.debug(s"Constrained prediction: ($q, $expected) => ${d.answerString}")
      Some(d)
    }
    case _ => {
      logger.debug(s"Could not make constrained predict for ($q, $expected)")
      None
    }
  }
  
  override def project(deriv: Deriv) = Set(deriv.answerString)
  
  private def normalize(s: String): String = Oracle.normalize(s)
  
  override def isCorrect(question: String, predicted: Set[String], expected: Set[String]) = predicted.toList match {
    case List(answer) => expected.map(normalize).contains(normalize(answer))
    case _ => false
  }

}

object MyTest extends App {
  val qa = QASystem.getInstance().get
  val generator = new AnswerGenerator {
    override def generateAnswers(q: String) = qa.deriveAnswers(q)
  }
  val featureSet = AnswerDerivationFeatures.featureMap
  val features = (d: AnswerDerivation) => {
    SparseVector(featureSet.map { case (name, f) => (name, f(d)) })
  }
  val weights = SparseVector()
  val model = QAModel(generator, features, weights)
  val p = new HiddenVariblePerceptron[String, AnswerDerivation, Set[String]]()
  val training = List(
      "What cures a cold?" -> Set("vitamin c", "oranges", "resting"),
      "How are lasers used?" -> Set("hair removal"),
      "Where was JFK shot?" -> Set("dallas", "Texas"),
      "Whats the capital of france?" -> Set("paris"),
      "What countries border with Germany?" -> Set("poland", "france", "swizerland", "austria", "belgium"),
      "Where was the pizza first invented?" -> Set("italy", "new york"),
      "What ingredients are in Indian food?" -> Set("cumin", "turmeric", "lime"),
      "Who invented radium?" -> Set("marie curie", "curie")
  )
  val newModel = p.train(model, training, 10)
  println(newModel.weights)
}