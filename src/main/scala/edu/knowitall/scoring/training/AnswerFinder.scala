package edu.knowitall.scoring.training

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig

/**
 * Wrapper for labeled (answer, question) pairs. Represents
 * a line of file input or output. 
 */
case class QAPair(label: String, answer: String, question: String, justification: String, confidence: Double) {
  def serialize = s"$label\t$answer\t$question\t$justification\t$confidence"
}
case object QAPair {
  def deserialize(str: String): QAPair = {
    val split = str.split("\t").map(_.trim)
    split match {
      case Array(label, answer, question, justification, confidence, _*) => QAPair(label, answer, question, justification, confidence.toDouble)
      case Array(label, answer, question, justification) => QAPair(label, answer, question, justification, -1)
      case Array(question)                => QAPair("X", "X", question, "X", -1)
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

  val maxAnswers = 1
  
  import AnalyzeTraining.getTopFieldValues
  import edu.knowitall.execution.Search
  import edu.knowitall.execution.Tuple
  import edu.knowitall.execution.Operators.Project
  import edu.knowitall.execution.Conditions.On
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
    
    def justField(str: String) = str.endsWith("arg1") || str.endsWith("rel") || str.endsWith("arg2") || str.endsWith("namespace")
    
    // run the question
    val systemAnswers = try { 
        system.answer(question).sortBy(-_.score).take(maxAnswers).map({ group =>
        val headTuple = group.derivations.head.etuple.tuple
        val cleanTuple = Tuple(headTuple.attrs.filter(p=>justField(p._1)))
        val answer = group.alternates.head.head
        (answer, QAPair("X", answer, question, cleanTuple.toString, group.score))
      }).toMap
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Map.empty[String, QAPair]
      }
    }
    
    // try to reuse existing answers
    val newAnswers = systemAnswers.toSeq.map { case (ans, pair) =>
      existingAnswers.find { 
        case QAPair(label, exAns, _, _, _) => exAns == ans && (label == "0" || label == "1") 
      } match { 
        case Some(existingPair) => pair.copy(label = existingPair.label)
        case None => pair
      }
    }
    
    if (newAnswers.nonEmpty) newAnswers else Seq(QAPair("X", "X", question, "X", -1))
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
  
  case class Config(
      inputFile: File = new File("."), 
      output: PrintStream = System.out, 
      parserName: String = "regex", 
      executor: String = "identity", 
      grouper: String = "basic",
      scorer: String = "logistic")
  
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f, "UTF8")) }
    opt[String]("parser") action { (f, c) => c.copy(parserName = f) }
    opt[String]("executor") action { (f, c) => c.copy(executor = f) }
    opt[String]("grouper") action { (f, c) => c.copy(grouper = f) }
    opt[String]("scorer") action { (f, c) => c.copy(scorer = f) }
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    val input = using(Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(QAPair.deserialize).toList
    }
    
    val sysConfig = QAConfig(config.parserName, config.executor, config.grouper, config.scorer)
    val system = QASystem.getInstance(sysConfig).get
    
    val answerFinder = new AnswerFinder(input, system)
    
    val output = answerFinder.output.map(_.serialize)
    
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}