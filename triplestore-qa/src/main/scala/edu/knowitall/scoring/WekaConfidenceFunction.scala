package edu.knowitall.scoring

import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet
import weka.classifiers.Classifier
import weka.core.Instances
import edu.knowitall.execution.AnswerGroup
import training.unweka.WekaTrainingReader.toUnlabeledInstance
import java.io.OutputStream

class WekaConfidenceFunction(
    featureSet: FeatureSet[AnswerGroup, Double], 
    classifier: Classifier,
    instances: Instances) extends ConfidenceFunction[AnswerGroup](featureSet) {
  
  def apply(group: AnswerGroup) = {
    val inst = toUnlabeledInstance(instances)(group)
    classifier.distributionForInstance(inst)(1)
  }
  
  def save(output: OutputStream): Unit = {
    throw new UnsupportedOperationException()
  }  
}