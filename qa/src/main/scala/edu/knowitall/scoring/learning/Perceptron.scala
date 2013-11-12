package edu.knowitall.scoring.learning

import scala.annotation.tailrec
import scala.util.Random
import edu.knowitall.util.Counter
import org.slf4j.LoggerFactory

class HiddenVariblePerceptron[Input, Hidden, Output](model: HiddenVariableModel[Input, Hidden, Output]) {
val logger = LoggerFactory.getLogger(this.getClass)

  private type Model = HiddenVariableModel[Input, Hidden, Output]
  private case class Example(input: Input, output: Output)
  
  private var currentModel = model
  private var currentSummedModel = model
  private var currentEpochCounts = new Counter()
  private var epochCounts = IndexedSeq.empty[Counter]
  
  private def updateModel(ex: Example, pred: Hidden) =  
    currentModel.predictConstrained(ex.input, ex.output) match {
      case Some(hidden) => {
        currentEpochCounts.increment("numUpdates")
        currentModel.update(ex.input, pred, hidden)
      }
      case None => {
        currentEpochCounts.increment("numMissedConstrained")
        currentModel
      }
    }
  
  private def evalPrediction(ex: Example, pred: Hidden) = {
    val proj = currentModel.project(pred)
    if (!currentModel.isCorrect(ex.input, proj, ex.output)) {
      updateModel(ex, pred)
    } else {
      currentEpochCounts.increment("numCorrect")
      currentModel
    }
  }
  
  private def trainIter(ex: Example) = {
    currentEpochCounts.increment("numInputs")
    currentModel.predict(ex.input) match {
      case None => currentModel
      case Some(pred) => {
        currentEpochCounts.increment("numPredicted")
        evalPrediction(ex, pred) 
      }
    }
  }
  
  private def trainEpoch(data: List[Example]): Unit = {
    data foreach {
      ex: Example => {
        currentModel = trainIter(ex)
        currentSummedModel = currentSummedModel.sum(currentModel)
      }
    }
  }
  
  private def computeEpochStats = {
    val numInputs = currentEpochCounts("numInputs")
    val numPreds = currentEpochCounts("numPredicted")
    val numCorrect = currentEpochCounts("numCorrect")
    val prec = if (numPreds > 0) numCorrect/numPreds else 0.0
    val rec = if (numInputs > 0) numCorrect/numInputs else 0.0
    val f1 = if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0.0
    currentEpochCounts.set("precision", prec)
    currentEpochCounts.set("recall", rec)
    currentEpochCounts.set("f1", f1)
  }
  
  private def trainHelper(data: List[(Input, Output)], numIters: Int) = {
    val examples = data map { case (in, out) => Example(in, out) }
    for (i <- 1 to numIters) {
      
      logger.debug(s"Starting epoch $i")
      trainEpoch(Random.shuffle(examples))
      computeEpochStats
      
      logger.debug(s"Epoch $i statistics: \n$currentEpochCounts")
      epochCounts = epochCounts :+ currentEpochCounts
      currentEpochCounts = new Counter()
    }
  }
  
  def getEpochCounts: IndexedSeq[Counter] = epochCounts
  
  def train(data: List[(Input, Output)], numIters: Int) = {
    trainHelper(data, numIters)
    currentModel
  }
  
  def trainAverage(data: List[(Input, Output)], numIters: Int) = {
    trainHelper(data, numIters)
    currentSummedModel.scale(1.0 / (numIters * data.size))
  }
  
}