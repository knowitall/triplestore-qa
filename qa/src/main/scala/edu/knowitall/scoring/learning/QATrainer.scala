package edu.knowitall.scoring.learning

import com.typesafe.config.ConfigFactory
import edu.knowitall.eval.FileOracle
import edu.knowitall.eval.Oracle
import scala.io.Source
import edu.knowitall.eval.FileOracle
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.AnswerDerivation

object QATrainer extends App {
  val conf = ConfigFactory.load()

  val labelsPath = args(0)
  val inputsPath = args(1)
  val modelOutput = args(2)
  val numIters = conf.getInt("perceptron.numIters")
  
  val oracle = new FileOracle(labelsPath)
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.map(Oracle.normalize).toList
  val training = oracle.toTrainingSet.filter(pair => inputs.contains(Oracle.normalize(pair._1)))
  
  val generator = QASystem.getInstance().get
  val model = QAModel(generator)
  val learner = new HiddenVariblePerceptron[String, AnswerDerivation, Set[String]]
  val learnedModel = learner.trainAverage(model, training, numIters)
  SparseVector.toFile(learnedModel.weights, modelOutput)

}