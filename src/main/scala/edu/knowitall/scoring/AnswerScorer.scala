package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.impl.LogisticRegression

trait AnswerScorer {
  def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup
}

trait ScoredAnswerGroup extends AnswerGroup {
  def score: Double
  def answer: String
  def alternates: List[String]
  def derivations: List[AnswerDerivation]
}

case class BasicScoredAnswer(answer: String, alternates: List[String], 
    derivations: List[AnswerDerivation], score: Double) 
    extends ScoredAnswerGroup
    
case class UniformAnswerScorer(s: Double = 0.0) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup) = 
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, s)
}

case class ClassifierScoredAnswer(
    answer: String, 
    alternates: List[String], 
    derivations: List[AnswerDerivation], 
    score: Double, 
    featureVector: Seq[(String, Double)]) extends ScoredAnswerGroup {
  
  def featureNames = featureVector.map(_._1)
  def featureValues = featureVector.map(_._2)
}

abstract class ClassifierAnswerScorer(val confFunction: ConfidenceFunction[AnswerGroup]) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup = {
    val featureNames = confFunction.featureSet.featureNames
    val featureValues = confFunction.featureSet.vectorize(group)
    require(featureNames.length == featureValues.length)
    val featureVector = featureNames.zip(featureValues)
    val confidence = confFunction
    ClassifierScoredAnswer(group.answer, group.alternates, group.derivations, confFunction(group), featureVector)
  }
}

class LogisticAnswerScorer(val logistic: LogisticRegression[AnswerGroup] = LogisticAnswerScorer.confFunction) extends ClassifierAnswerScorer(logistic)

object LogisticAnswerScorer {

  val confFunction = {
    val featureSet = AnswerGroupFeatures.featureSet
    // Stub: random feature weights for now.
    val random = new scala.util.Random(17)
    val randomWeights = Seq.fill(featureSet.numFeatures) { random.nextDouble }
    val weightsMap = featureSet.featureNames.zip(randomWeights).toMap
    val intercept = random.nextDouble
    new LogisticRegression(featureSet, weightsMap, intercept)
  }
}