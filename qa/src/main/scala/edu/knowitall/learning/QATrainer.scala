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

object QATrainer extends App {

  val conf = ConfigFactory.load()

  val labelsPath = args(0)
  val inputsPath = args(1)  
  val dir = new File(args(2))
  if (dir.exists() && !dir.isDirectory()) 
    throw new IllegalStateException(s"$dir exists but is not a directory")
  if (!dir.exists()) dir.mkdirs()
  val modelOutput = new File(dir, "model.txt")
  
  val configOutput = new PrintWriter(new File(dir, "config.txt"))
  configOutput.write(conf.root().render)
  configOutput.close()
  
  val numIters = conf.getInt("perceptron.numIters")
  
  val oracle = new MemoryInteractiveOracle(labelsPath)
  val inputs = Source.fromFile(inputsPath, "UTF8").getLines.map(Oracle.normalize).toList
  
  val model = QaModel()
  
  val learner = new Perceptron(model, oracle)
  println("Learning...")
  val learnedModel = learner.learn(inputs)
  println("Done learning")
  
  SparseVector.toFile(model.costModel.weights, modelOutput.toString())

}