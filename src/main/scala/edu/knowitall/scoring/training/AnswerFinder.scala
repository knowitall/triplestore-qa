package edu.knowitall.scoring.training

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig

/**
 * Wrapper for labeled (answer, question) pairs. Represents
 * a line of file input or output. 
 */
case class QAPair(label: String, answer: String, question: String, justification: String) {
  def serialize = s"$label\t$answer\t$question\t$justification"
}
case object QAPair {
  def deserialize(str: String): QAPair = {
    val split = str.split("\t").map(_.trim)
    split match {
      case Array(label, answer, question, justification, _*) => QAPair(label, answer, question, justification)
      case Array(question)                => QAPair("X", "X", question, "X")
      case _ => throw new IllegalArgumentException(s"Invalid QAPair: $str")
    }
  }
}

/**
 * Takes as input:
 * 1. Seq[QAPair]
 * 2. QASystem
 * 
 * Runs each qapair's question through QASystem to get a set of answers as output.
 * Any new answers are appended to the input QAPairs with a blank or null label.
 * Labels for existing answers are preserved. 
 */
class AnswerFinder(val input: Seq[QAPair], val system: QASystem) {

  import AnalyzeTraining.getTopFieldValues
  import edu.knowitall.execution.Search
  import edu.knowitall.execution.AnswerGroup
  
  def output: Seq[QAPair] = {
    
    val qaSets = input.groupBy(_.question)
    
    val allAnswers = qaSets.iterator.toSeq.flatMap { 
      case (question, existingAnswers) => findNewAnswers(question, existingAnswers) 
    }
    
    // put unlabeled data at the bottom
    val unlabeledWithAnswers = allAnswers.filter(p => p.label == "X" && p.answer != "X").sortBy(_.question)
    val unlabeledWithoutAnswers = allAnswers.filter(p => p.label == "X" && p.answer == "X").sortBy(_.question)
    val labeled = allAnswers.filterNot(_.label == "X").sortBy(_.question)
    (labeled ++ unlabeledWithAnswers ++ unlabeledWithoutAnswers).toSeq
  }
  
  private def getTopTuple(group: AnswerGroup): String = {
    val topArg1s = getTopFieldValues(group, Search.arg1, 1)
    val topRels  = getTopFieldValues(group, Search.rel, 1)
    val topArg2s  = getTopFieldValues(group, Search.arg2, 1)
    (topArg1s ++ topRels ++ topArg2s).mkString("(", ", ", ")")
  }
  
  private def findNewAnswers(question: String, existingAnswers: Seq[QAPair]): Seq[QAPair] = {
    
    // run the question
    val systemAnswersJustifications = system.answer(question).map({ 
      group => (group.alternates.head.head, getTopTuple(group)) 
    }).toMap
    
    val existingAnswerStrings = existingAnswers.map(_.answer).toSet
    
    val newAnswerStrings = systemAnswersJustifications.keySet.filterNot(ans => existingAnswerStrings.contains(ans))
    
    val newAnswers = newAnswerStrings.map(ans => QAPair("X", ans, question, systemAnswersJustifications(ans)))
    
    val combined = (existingAnswers ++ newAnswers).take(50)
    // if combined has size > 1 then we filter out the placeholder "X" answer.
    if (combined.size > 1) {
      combined.filterNot(_.answer == "X")
    } else {
      combined
    }
  }
}

/**
 * Finds answers for an evaluation set.
 */
object EvaluationAnswerFinder extends App {
 
  import scopt.OptionParser
  import java.io.{File, PrintStream}
  import edu.knowitall.common.Resource.using
  import scala.io.Source
  
  case class Config(inputFile: File = new File("."), output: PrintStream = System.out)
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f, "UTF8")) }
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    val input = using(Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(QAPair.deserialize).toList
    }
    
    val sysConfig = QAConfig("keyword", "identity", "basic", "logistic")
    val system = QASystem.getInstance(sysConfig).get
    
    val answerFinder = new AnswerFinder(input, system)
    
    val output = answerFinder.output.map(_.serialize)
    
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}