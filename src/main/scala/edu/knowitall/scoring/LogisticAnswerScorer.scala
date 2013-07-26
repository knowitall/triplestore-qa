package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.impl.LogisticRegression

abstract class ClassifierAnswerScorer(val confFunction: ConfidenceFunction[AnswerGroup]) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup = {
    val featureNames = confFunction.featureSet.featureNames
    val featureValues = confFunction.featureSet.vectorize(group)
    require(featureNames.length == featureValues.length)
    val featureVector = featureNames.zip(featureValues)
    val confidence = confFunction
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, confFunction(group))
  }
}

class LogisticAnswerScorer(
    val logistic: LogisticRegression[AnswerGroup] = 
      LogisticAnswerScorer.defaultClassifier) 
extends ClassifierAnswerScorer(logistic)

object LogisticAnswerScorer {
  
  val defaultModel = "logistic-scorer.model"
    
  val defaultModelURL = {
    val url = getClass.getResource(defaultModel)
    require(url != null, "Could not find " + defaultModel)
    url
  }
  
  val defaultClassifier = LogisticRegression.fromUrl(AnswerGroupFeatures.featureSet, defaultModelURL)
  
}