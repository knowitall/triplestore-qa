package edu.knowitall.scoring.eval

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig
import AnalyzeTraining.getTopFieldValues
import edu.knowitall.common.Resource.using
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.Search
import edu.knowitall.execution.Tuple
import java.io.File
import java.io.PrintStream
import scala.Array.canBuildFrom
import scala.io.Source
import scopt.OptionParser
import edu.knowitall.scoring.eval.AnalyzeTraining

/**
 * Wrapper for paraphrase sets that are optionally labeled, answered, justified, confidenced.
 */
case class ParaphraseSet(labelStr: String, answerStr: String, justificationStr: String, confidenceStr: String, questions: Set[String]) {
  
  def ifNotX(s: String) = if (s == "X") None else Some(s)
  
  def label = ifNotX(labelStr)
  def answer = ifNotX(answerStr)
  def justification = ifNotX(justificationStr)
  def confidence = ifNotX(confidenceStr).map(_.toDouble)
  
  def serialize = {
    val fields = Seq(labelStr, answerStr, justificationStr, confidenceStr) ++ questions.toSeq
    fields.mkString("\t")
  }
}
case object ParaphraseSet {
  def deserialize(str: String): ParaphraseSet = {
    val split = str.split("\t").map(_.trim).toList
    split match {
      case labelStr :: answerStr :: justificationStr :: confidenceStr :: questions => {
        require(questions.nonEmpty, "Empty question set.")
        ParaphraseSet(labelStr, answerStr, justificationStr, confidenceStr, questions.toSet)
      }
      case _ => throw new IllegalArgumentException(s"Invalid ParaphraseSet: $str")
    }
  }
  
  def fromParaphrases(str: String): ParaphraseSet = {
    val split = str.split("\t").map(_.trim).toList
    ParaphraseSet("X", "X", "X", "X", split.toSet)
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
class EvalAnswerFinder(val input: Seq[ParaphraseSet], val system: QASystem, val allParaphrases: Boolean) {

  import AnalyzeTraining.getTopFieldValues
  import edu.knowitall.execution.Search
  import edu.knowitall.execution.Tuple
  import edu.knowitall.execution.Operators.Project
  import edu.knowitall.execution.Conditions.On
  import edu.knowitall.execution.AnswerGroup
  
  def output: Seq[ParaphraseSet] = {
    
    val pSets = input.groupBy(_.questions)
    
    val allAnswers = pSets.iterator.toSeq.map { 
      case (questions, existingAnswers) => findNewAnswers(questions, existingAnswers) 
    }
    
    // put unlabeled data at the bottom
    val unlabeledWithAnswers = allAnswers.filter(p => p.label.isEmpty && p.answer.nonEmpty).groupBy(_.questions).flatMap(_._2)
    val unlabeledWithoutAnswersOrJustification = allAnswers.filter(p => p.label.isEmpty && p.answer.isEmpty && p.justification.isEmpty).groupBy(_.questions).flatMap(_._2)
    val unlabeledWithoutAnswersWithJustification = allAnswers.filter(p => p.label.isEmpty && p.answer.isEmpty && p.justification.nonEmpty).groupBy(_.questions).flatMap(_._2)
    val labeled = allAnswers.filterNot(_.label.isEmpty).groupBy(_.questions).flatMap(_._2)
    (labeled ++ unlabeledWithAnswers ++ unlabeledWithoutAnswersWithJustification ++ unlabeledWithoutAnswersOrJustification).toSeq
  }
  
  private def getTopTuple(group: AnswerGroup): String = {
    val topArg1s = getTopFieldValues(group, Search.arg1, 1)
    val topRels  = getTopFieldValues(group, Search.rel, 1)
    val topArg2s  = getTopFieldValues(group, Search.arg2, 1)
    (topArg1s ++ topRels ++ topArg2s).mkString("(", ", ", ")")
  }
  
  private def findNewAnswers(questions: Set[String], existingAnswers: Seq[ParaphraseSet]): ParaphraseSet = {
    
    def justField(str: String) = str.endsWith("arg1") || str.endsWith("rel") || str.endsWith("arg2") || str.endsWith("namespace")

    // run the question
    val newPSet = {
      val topQAnswer = try {
        val questionsConsidered = if (allParaphrases) questions else questions.toSeq.take(1).toSet
        questions.toSeq.flatMap(q => system.answer(q).map(a => (q, a))).sortBy(-_._2.score).take(1).toSeq.headOption
      } catch {
        case e: Exception => { e.printStackTrace(); None }
      }
      topQAnswer match {
        case Some((question, group)) => {
          val headTuple = group.derivations.head.etuple.tuple
          val cleanTuple = Tuple(headTuple.attrs.filter(p => justField(p._1)))
          val answer = group.alternates.head.head
          ParaphraseSet("X", answer, question + " " + cleanTuple.toString, group.score.toString, questions)
        }
        case None => {
          val just = questions.flatMap(q => (system.parser.parse(q).map(qu => q + " " + qu.toString))).headOption.getOrElse("X")
          ParaphraseSet("X", "X", just, "X", questions)
        }
      }
    }
    
    // try to reuse existing answers
    val existingAnswer = existingAnswers.find(pset =>
      pset.label.isDefined &&
      pset.answer.isDefined && 
      pset.answer == newPSet.answer && 
      pset.questions.equals(newPSet.questions))
    existingAnswer match {
      case Some(pset) => newPSet.copy(labelStr = pset.labelStr)
      case None => newPSet
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
  
  case class Config(
      inputFile: File = new File("."), 
      inputRaw: Boolean = false,
      allParaphrases: Boolean = true,
      output: PrintStream = System.out, 
      parserName: String = "regex", 
      executor: String = "identity", 
      grouper: String = "basic",
      scorer: String = "logistic")
  
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f, "UTF8")) }
    opt[Boolean]("allParaphrases") action { (b, c) => c.copy(allParaphrases = b) }
    opt[Boolean]("inputType") action { (b, c) => c.copy(inputRaw = b) }
    opt[String]("parser") action { (f, c) => c.copy(parserName = f) }
    opt[String]("executor") action { (f, c) => c.copy(executor = f) }
    opt[String]("grouper") action { (f, c) => c.copy(grouper = f) }
    opt[String]("scorer") action { (f, c) => c.copy(scorer = f) }
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    def deserializer = if (!config.inputRaw) ParaphraseSet.deserialize _ else ParaphraseSet.fromParaphrases _
    
    val input = using(Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(deserializer).toList
    }
    
    val sysConfig = QAConfig(config.parserName, config.executor, config.grouper, config.scorer)
    val system = QASystem.getInstance(sysConfig).get
    
    val answerFinder = new EvalAnswerFinder(input, system, config.allParaphrases)
    
    val output = answerFinder.output.map(_.serialize)
    
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}