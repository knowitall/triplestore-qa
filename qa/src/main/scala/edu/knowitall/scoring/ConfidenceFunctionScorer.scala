package edu.knowitall.scoring

import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.impl.LogisticRegression
import edu.knowitall.scoring.features.AnswerGroupFeatures

abstract class ConfidenceFunctionScorer extends AnswerScorer {
  
  def confFunction: ConfidenceFunction[AnswerGroup]
  
  def featureSet: FeatureSet[AnswerGroup, Double]
  
  override def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup = {
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, confFunction(group))
  }
}


case class LogisticAnswerScorer() extends ConfidenceFunctionScorer {
  
  val defaultModel = "logistic-scorer.model"
    
  val defaultModelURL = {
    val url = getClass.getResource(defaultModel)
    require(url != null, "Could not find " + defaultModel)
    url
  }
  
  override val featureSet = AnswerGroupFeatures
  
  override val confFunction = LogisticRegression.fromUrl(AnswerGroupFeatures, defaultModelURL)
}

case class DecisionTreeScorer() extends ConfidenceFunctionScorer {
  
  private val defaultTraining = training.TrainingDataReader.defaultTraining
  
  override val featureSet = AnswerGroupFeatures
  
  override val confFunction  = training.J48Trainer.train(defaultTraining)
}