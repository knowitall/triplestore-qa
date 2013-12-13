package edu.knowitall.learning

import com.typesafe.config.ConfigFactory
import edu.knowitall.eval.FileOracle
import edu.knowitall.eval.Oracle
import scala.io.Source
import edu.knowitall.eval.FileOracle
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.AnswerDerivation
import java.io.File
import java.io.PrintWriter
import edu.knowitall.util.Counter
import edu.knowitall.model.QaModel
import edu.knowitall.model.Derivation
import java.text.SimpleDateFormat
import java.util.Calendar

class QaTrainer(model: QaModel, oracle: CorrectnessModel[String, Derivation]) extends HiddenVariableModel[String, Derivation] {
  
  private var avgWeights = model.costModel.weights
  private var iter = 1.0
  private val perceptron = new Perceptron(this, oracle)
  
  override def predict(question: String) = model.predict(question)
  
  override def candidatePredictions(question: String) = model.candidatePredictions(question)
  
  override def update(question: String, predicted: Derivation, expected: Derivation) = {
    model.update(question, predicted, expected)
    avgWeights = avgWeights + (expected.features - predicted.features) * iter
  }
  
  def learnIter(question: String) = {
    perceptron.learnIter(question)
    iter += 1
  }
  
  def learn(inputs: Traversable[String]) = inputs foreach learnIter
  
  def averagedWeights = model.costModel.weights - (avgWeights / iter)
  
}

object QaTrainer extends App {
  
  def timestamp = {
    val fmt = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss")
    val today = Calendar.getInstance.getTime
    fmt.format(today)
  }

  val conf = ConfigFactory.load()
  
  val oracleMode = conf.getString("learning.oracleMode")
  val labelsPath = conf.getString("learning.labelsPath")
  val inputsPath = conf.getString("learning.inputsPath")
  val outputsPath = conf.getString("learning.outputsPath")
  val numIters = conf.getInt("learning.numIters")
  val runName = if (conf.hasPath("learning.runName")) conf.getString("learning.runName") else "unnamed"

      
  val dir = new File(outputsPath, s"${runName}-${timestamp}")
  if (dir.exists() && !dir.isDirectory()) 
    throw new IllegalStateException(s"$dir exists but is not a directory")
  if (!dir.exists()) dir.mkdirs()
  val modelOutput = new File(dir, "model.txt")
  
  val configOutput = new PrintWriter(new File(dir, "config.txt"))
  configOutput.write(conf.root().render)
  configOutput.close()
  
  
  
  val oracle = oracleMode match {
    case "interactive" => new MemoryInteractiveOracle(labelsPath)
    case "file" => new LabeledDataOracle(labelsPath)
    case _ => throw new IllegalStateException(s"Invalid oracle mode: $oracleMode")
  }
  
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.map(Oracle.normalize).toList
  
  val model = QaModel()
  
  val trainer = new QaTrainer(model, oracle)
  println("Learning...")
  for (i <- 1 to numIters) {
    trainer.learn(inputs)
    val file = new File(dir, s"model.$i.txt")
    SparseVector.toFile(model.costModel.weights, file.toString())
    val avgFile = new File(dir, s"model.$i.avg.txt")
    SparseVector.toFile(trainer.averagedWeights, avgFile.toString())
  }
  println("Done learning")
  
  SparseVector.toFile(model.costModel.weights, modelOutput.toString())
  SparseVector.toFile(trainer.averagedWeights, (new File(dir, s"model.avg.txt")).toString())

}