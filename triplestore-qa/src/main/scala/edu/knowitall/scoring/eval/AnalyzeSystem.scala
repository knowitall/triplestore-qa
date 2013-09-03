package edu.knowitall.scoring.eval

import edu.knowitall.common.Resource.using
import java.io.File
import java.io.PrintStream
import scopt.OptionParser

sealed trait EvalMode
case object OneAnswer extends EvalMode // Prec/Recall @ 1 - Only consider a single top-ranked answer per question
case object Aggregate extends EvalMode // Consider all answers per question

class AnalyzeSystem(val mode: EvalMode, val input: Seq[ParaphraseSet]) {
  
  def precision: Seq[Double] = {
    val booleans = input.filter(_.confidence.isDefined).sortBy(-_.confidence.get).map(_.label.exists(_ == "1"))
    AnalyzeClassifier.precRecall(booleans)
  }
}

object AnalyzeSystem extends App {
  
  import scopt.OptionParser
  import java.io.{File, PrintStream}
  import edu.knowitall.common.Resource.using
  
  case class Config(inputFile: File = new File("."), 
      output: PrintStream = System.out, 
      singleAnswer: Boolean = false)
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f)) }
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    val input = using(io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(ParaphraseSet.deserialize).toList  
    }
    
    input foreach println

    val mode = if (config.singleAnswer) OneAnswer else Aggregate
    
    val analyzer = new AnalyzeSystem(mode, input)
    
    val output = analyzer.precision.zipWithIndex.map {
      case (prec, index) => s"$index\t$prec"
    }
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}