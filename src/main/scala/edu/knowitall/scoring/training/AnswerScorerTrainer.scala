package edu.knowitall.scoring.training

import edu.knowitall.tool.conf.impl.LogisticRegression
import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.scoring.AnswerGroupFeatures
import edu.knowitall.tool.conf.Labelled
import java.io.FileOutputStream

object AnswerScorerTrainer {
  
  def trainer = new BreezeLogisticRegressionTrainer[AnswerGroup](AnswerGroupFeatures.featureSet)
  
  
  def trainClassifier(training: Iterable[Labelled[AnswerGroup]]) = {
    val classifier = trainer.train(training)
    val weights = classifier.featureWeights.iterator
    weights.foreach { case (feature, weight) =>
      System.err.println(s"$feature\t$weight")  
    }
    classifier
  }
  
  lazy val loadDefaultClassifier = trainClassifier(TrainingDataReader.defaultTraining)
  
  def main(args: Array[String]): Unit = {
    
    val savePath = args(0)
    
    val outStream = new FileOutputStream(savePath)
    
    loadDefaultClassifier.save(outStream)
    
    outStream.close()
  }
}