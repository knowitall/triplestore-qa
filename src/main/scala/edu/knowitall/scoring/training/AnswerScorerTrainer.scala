package edu.knowitall.scoring.training

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.impl.LogisticRegression
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.scoring.features.AnswerGroupFeatures
import edu.knowitall.tool.conf.Labelled
import java.io.FileOutputStream

case object LogisticTrainer extends ConfidenceTrainer[AnswerGroup](AnswerGroupFeatures) {
  
  def trainer = new BreezeLogisticRegressionTrainer[AnswerGroup](features)
  
  def train(training: Iterable[Labelled[AnswerGroup]]): LogisticRegression[AnswerGroup] = {
    val classifier = trainer.train(training)
    val weights = classifier.featureWeights.iterator
    weights.foreach { case (feature, weight) =>
      System.err.println(s"$feature\t$weight")  
    }
    classifier
  }
  
  lazy val loadDefaultClassifier = train(TrainingDataReader.defaultTraining)
  
  def main(args: Array[String]): Unit = {
    
    val savePath = args(0)
    
    val outStream = new FileOutputStream(savePath)
    
    loadDefaultClassifier.save(outStream)
    
    outStream.close()
  }
}

object J48Trainer extends ConfidenceTrainer[AnswerGroup](AnswerGroupFeatures) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.J48
  import unweka.WekaTrainingReader 
  
  def train(training: Iterable[Labelled[AnswerGroup]]): WekaConfidenceFunction = {
    val instances = WekaTrainingReader.toInstances(training) 
    val j48 = new weka.classifiers.trees.J48()
    val options = Array("-R", "-Q 1") 
    j48.setOptions(options)
    j48.buildClassifier(instances)
    new WekaConfidenceFunction(features, j48, instances)
  }
}

object WekaLogisticTrainer extends ConfidenceTrainer[AnswerGroup](AnswerGroupFeatures) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.functions.Logistic
  import unweka.WekaTrainingReader 
  
  def train(training: Iterable[Labelled[AnswerGroup]]): WekaConfidenceFunction = {
    val instances = WekaTrainingReader.toInstances(training) 
    val logistic = new Logistic()
    logistic.setOptions(Array("-D"))
    logistic.buildClassifier(instances)
    new WekaConfidenceFunction(features, logistic, instances)
  }
}

