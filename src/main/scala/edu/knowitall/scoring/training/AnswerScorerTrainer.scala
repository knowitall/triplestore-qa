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
    // -R reduced error pruning
    // -Q 1 random seed
    // -A laplace smoothing probabilities
    val options = Array("-R", "-Q 1", "-A") 
    j48.setOptions(options)
    j48.buildClassifier(instances)
    new WekaConfidenceFunction(features, j48, instances)
  }
}

object REPTreeTrainer extends ConfidenceTrainer[AnswerGroup](AnswerGroupFeatures) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.REPTree
  import unweka.WekaTrainingReader 
  
  def train(training: Iterable[Labelled[AnswerGroup]]): WekaConfidenceFunction = {
    val instances = WekaTrainingReader.toInstances(training) 
    val repTree = new weka.classifiers.trees.REPTree()
    // -S 1 random seed
    val options = Array("-S 1") 
    repTree.setOptions(options)
    repTree.buildClassifier(instances)
    new WekaConfidenceFunction(features, repTree, instances)
  }
}

object RandomForestTrainer extends ConfidenceTrainer[AnswerGroup](AnswerGroupFeatures) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.RandomForest
  import unweka.WekaTrainingReader 
  
  def train(training: Iterable[Labelled[AnswerGroup]]): WekaConfidenceFunction = {
    val instances = WekaTrainingReader.toInstances(training) 
    val randomForest = new weka.classifiers.trees.RandomForest()
    // -I 10 num trees
    // -K num features
    // -S random seed
    val options = s"-I 10 -K ${features.numFeatures/2} -S 1".split(" ")
    randomForest.setOptions(options)
    randomForest.buildClassifier(instances)
    new WekaConfidenceFunction(features, randomForest, instances)
  }
}

