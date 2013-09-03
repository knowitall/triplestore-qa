package edu.knowitall.scoring.eval

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.execution.AnswerGroup
import scala.util.Random
import edu.knowitall.scoring.training.LogisticTrainer
import edu.knowitall.scoring.training.TrainingDataReader

case class Scored[T](item: T, score: Double)

object AnalyzeClassifier {

  val rand = new Random(0)
  
  lazy val allTrainingData = TrainingDataReader.defaultTraining.toSet
  
  def precRecall(sorted: Seq[Boolean]): Seq[Double] = {
    
    var result: List[Double] = Nil
    
    var total = 0
    var correct = 0
    
    for (label <- sorted) {
      total += 1
      if (label) {
        correct += 1
        result ::= (correct.toDouble / total.toDouble)
      }
    }
    result.reverse.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
  }
 
  type TrainingSet = Set[Labelled[AnswerGroup]]
  type TestSet     = Set[Labelled[AnswerGroup]]
  
  def crossValidationSplits(splits: Int): Seq[(TrainingSet, TestSet)] = {
    
    val testSize = math.ceil(allTrainingData.size.toDouble / splits.toDouble).toInt
    
    val shuffledData = rand.shuffle(allTrainingData.toSeq)
    
    val testSets: Seq[TestSet] = shuffledData.grouped(testSize).map(_.toSet).toSeq
    
    val trainingSets = testSets.map(tset => allTrainingData &~ tset)
    
    trainingSets.zip(testSets).toSeq
  }
  
  type ScoredItem = Scored[Labelled[AnswerGroup]]
  
  def eval(trainingSet: TrainingSet, testSet: TestSet): Set[ScoredItem] = {

    val classifier = LogisticTrainer.train(trainingSet)
    def scoreDatum(datum: Labelled[AnswerGroup]) = Scored(datum, classifier(datum.item))
    val scoredItems = testSet map scoreDatum
    scoredItems
  }
  
  def evalCrossValidation(numSplits: Int): Seq[Double] = {
    
    val dataSplits = crossValidationSplits(numSplits)
    val allScoredItems = dataSplits.flatMap({ case (training, test) => eval(training, test) }) 
    val sortedScoredItems = allScoredItems.toList.sortBy(-_.score)
    val sortedBooleans = sortedScoredItems.map(_.item.label)
    val precisionCurve = precRecall(sortedBooleans)
    precisionCurve
  }
  
  def main(args: Array[String]): Unit = {
    
    evalCrossValidation(10).zipWithIndex foreach { case (prec, recall) =>
      println(s"$recall\t$prec")  
    }
  }
}