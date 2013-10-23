package edu.knowitall.scoring.learning

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.scoring.features.AnswerDerivationFeatures

case class Featurized[X, Y](input: X, hidden: Y, values: Map[String, Double]) {
  def diff(other: Featurized[X, Y]): Map[String, Double] = {
    val keys = this.values.keySet ++ other.values.keySet
    val newValues = for (k <- keys;
    					 v1 = this.values.getOrElse(k, 0.0);
    					 v2 = other.values.getOrElse(k, 0.0))
    				  yield (k, v1 - v2)
    newValues.toMap
  }
}

trait FeatureFunction[X, Y] {
  def featurize(x: X, y: Y): Featurized[X, Y]
  def apply(x: X, y: Y) = featurize(x, y)
}

trait FeatureWeights {
  def weight(name: String): Double
  def update(name: String, value: Double)
  def update(map: Map[String, Double])
}

class WeightMap() extends FeatureWeights {
  val values = scala.collection.mutable.Map[String, Double]()
  override def weight(name: String) = values.getOrElse(name, 0.0)
  override def update(name: String, value: Double) = {
    values(name) = weight(name) + value
  }
  override def update(map: Map[String, Double]) = for ((k, v) <- map) update(k, v)
  override def toString = values.toString
}

class QAFeatureFunction extends FeatureFunction[String, AnswerDerivation] {
  override def featurize(question: String, deriv: AnswerDerivation) = {
    val values = for ((name, func) <- AnswerDerivationFeatures.featureMap) yield (name, func(deriv))
    Featurized(question, deriv, values.toMap)
  }
}

abstract class Perceptron[X, Y, Z] {
  def weights: FeatureWeights
  def features: FeatureFunction[X, Y]
  def correct(z1: Z, z2: Z): Boolean
  def project(y: Y): Z
  def predictHidden(x: X): Option[Y]
  def predictHiddenConstrained(x: X, z: Z): Option[Y]
  
  def score(x: X, y: Y): Double = {
    val f = features(x, y)
    val dot = for ((featureName, featureValue) <- f.values; featureWeight = weights.weight(featureName)) yield featureWeight * featureValue
    dot.sum
  }
  
  def update(x: X, yCorrect: Y, yIncorrect: Y) = {
    val fCorrect = features(x, yCorrect)
    val fIncorrect = features(x, yIncorrect)
    val diff = fCorrect.diff(fIncorrect)
    weights.update(diff)
  }
  def trainIter(x: X, zCorrect: Z) = {
    for (yPredicted    <- predictHidden(x);
    	 zPredicted    =  project(yPredicted);
    	 if !correct(zPredicted, zCorrect);
    	 yCorrect      <- predictHiddenConstrained(x, zCorrect))
      update(x, yCorrect, yPredicted)
  }
  def train(examples: List[(X, Z)], numIters: Int = 1) = {
    for (i <- 1 to numIters; (x, z) <- examples) trainIter(x, z)
    weights
  }
}

class QAPerceptron() extends Perceptron[String, AnswerDerivation, Set[String]] {
  
  val qa = QASystem.getInstance().get
  override val weights = new WeightMap()
  override val features = new QAFeatureFunction()
  
  override def correct(z1: Set[String], z2: Set[String]) = z1.toList match {
    case List(answer) => z2.contains(answer)
    case _ => false
  }
  
  override def project(derivation: AnswerDerivation) = Set(derivation.answerString)
  
  override def predictHidden(question: String) = qa.deriveAnswers(question) match {
    case derivs if derivs.size > 0 => Some(derivs.maxBy(deriv => score(question, deriv)))
    case _ => None
  } 
  
  override def predictHiddenConstrained(question: String, answerSet: Set[String]) = {
    val derivs = qa.deriveAnswers(question)
    val correctDerivs = derivs.filter(d => correct(Set(d.answerString), answerSet))
    correctDerivs match {
      case derivs if derivs.size > 0 => Some(derivs.maxBy(d => score(question, d)))
      case _ => None
    }
  }
  
}

object MyTest extends App {
  val training = List(("what treats a cold?", Set("chicken soup", "rest")))
  val perceptron = new QAPerceptron()
  val weights = perceptron.train(training, 5)
  println(weights.toString)
}