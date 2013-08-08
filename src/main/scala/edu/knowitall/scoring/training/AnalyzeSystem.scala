package edu.knowitall.scoring.training

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig

sealed trait EvalMode
case object OneAnswer extends EvalMode // Prec/Recall @ 1 - Only consider a single top-ranked answer per question
case object Aggregate extends EvalMode // Consider all answers per question

class AnalyzeSystem(val mode: EvalMode, val input: Seq[QAPair], val system: QASystem) {
  
  def precision: Seq[Double] = {
    val booleans = input.sortBy(-_.confidence).map(_.label == "1")
    AnalyzeClassifier.precRecall(booleans)
  }
}

object AnalyzeSystem extends App {
  
  import scopt.OptionParser
  import java.io.{File, PrintStream}
  import edu.knowitall.common.Resource.using
  
  case class Config(inputFile: File = new File("."), 
      output: PrintStream = System.out, 
      singleAnswer: Boolean = true,
      parserName: String = "regex",
      executor: String = "identity",
      grouper: String = "basic",
      scorer: String = "logistic")
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    arg[String]("parser") action { (f, c) => c.copy(parserName = f) }
    arg[String]("executor") action { (f, c) => c.copy(executor = f) }
    arg[String]("grouper") action { (f, c) => c.copy(grouper = f) }
    arg[String]("scorer") action { (f, c) => c.copy(scorer = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f)) }
    opt[Boolean]("multiAnswer") action { (b, c) => c.copy(singleAnswer = !b) } text("Consider all answers per question (default: consider only top answer")
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    val input = using(io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(QAPair.deserialize).toList  
    }
    
    val sysConfig = QAConfig(config.parserName, config.executor, config.grouper, config.scorer)
    val system = QASystem.getInstance(sysConfig).get
    
    val mode = if (config.singleAnswer) OneAnswer else Aggregate
    
    val analyzer = new AnalyzeSystem(mode, input, system)
    
    val output = analyzer.precision.zipWithIndex.map {
      case (prec, index) => s"$index\t$prec"
    }
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}