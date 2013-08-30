package edu.knowitall.scoring.training

import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.common.Resource.using
import weka.classifiers.Classifier
import weka.core.Instances
import edu.knowitall.execution.AnswerGroup
import unweka.WekaTrainingReader.toUnlabeledInstance
import java.io.OutputStream
import java.io.File

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