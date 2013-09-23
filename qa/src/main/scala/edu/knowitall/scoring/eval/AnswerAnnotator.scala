package edu.knowitall.scoring.eval

import java.io.File
import scopt.OptionParser
import edu.knowitall.common.Resource.using
import scala.io.Source
import java.io.PrintStream
import edu.knowitall.apps.QAConfig
import edu.knowitall.apps.QASystem
import edu.knowitall.util.TuplePrinter

/**
 * Reads in a list of questions and annotates them with answers. Can optionally read in it's own output
 * format and augment it with additional answers.
 */
object AnswerAnnotator {
  
  case class AnswerJustification(label: String, answer: String, justification: String, query: String)
  
  case class Question(question: String, answerJustification: Option[AnswerJustification]) {
    override def toString(): String = {
      answerJustification match {
        case Some(AnswerJustification(label, answer, justification, query)) => Seq(label, answer, question, justification, query).mkString("\t")
        case None => Seq("X", "X", question, "X", "X").mkString("\t")
      }
    }

    // Attempts to parse the question
    def formatString(config: Config): String = {
      answerJustification match {
        case Some(AnswerJustification(label, answer, justification, query)) => Seq(label, answer, question, justification, query).mkString("\t")
        case None => {
          val parsedQuery = config.system.parser.parse(question).headOption.map(_.toString).getOrElse("X")
          Seq("X", "X", question, "X", parsedQuery).mkString("\t")
        }
      }
    }
  }
  
  object Question {
    def fromString(str: String): Question = {
      str.split("\t") match {
        case Array("X", "X", question, "X", _, _*) => Question(question, None)
        case Array(label, answer, question, justification, query, _*) => Question(question, Some(AnswerJustification(label, answer, justification, query)))
        case Array(question) => Question(question, None)
        case _ => throw new RuntimeException("Unrecognized input record: "+str)
      }
    }
  }
  
  case class Config(
      inputFile: File = new File("."), 
      outputFile: File = new File("."),
      numAnswers: Int = 1,
      parser: String   = "regex",
      executor: String = "identity",
      grouper: String  = "basic",
      scorer: String   = "logistic") {
    
	  lazy val sysConfig = QAConfig(parser, executor, grouper, scorer)
      lazy val system = QASystem.getInstance(sysConfig).get 
  }

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("AnswerAnnotator") {
      arg[File]("inputFile")  action { (f, c) => c.copy(inputFile = f) }
      arg[File]("outputFile") action { (f, c) => c.copy(outputFile = f) }
      opt[Int]("numAnswers")  action { (f, c) => c.copy(numAnswers = f) } text("Number of new answers to add")
      opt[String]("parser")   action { (s, c) => c.copy(parser = s) }
      opt[String]("executor") action { (s, c) => c.copy(executor = s) }
      opt[String]("basic")    action { (s, c) => c.copy(grouper = s) }
      opt[String]("scorer") action { (s, c) => c.copy(scorer = s) }
    }

    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def run(config: Config): Unit = {
    val questions = using(Source.fromFile(config.inputFile, "UTF8")) { input =>
      input.getLines.map(Question.fromString).toList
    }
    using(new PrintStream(config.outputFile, "UTF8")) { output =>

      answerQuestions(config, questions) foreach { q => output.println(q.formatString(config)) }
    }
  }
  
  def answerQuestions(config: Config, questions: List[Question]): Seq[Question] = {
    // deserialize questions
    // group by question and answer strings
    val questionsGrouped = questions.groupBy(_.question).map { case (qString, qs) => 
      val ajs = qs.flatMap(_.answerJustification)
      (qString, ajs.map(a => (a.answer, a)).toMap)
    } 
    // try to find previously unknown answers
    val moreAnswers = questionsGrouped.iterator.map { case (qString, ajs) => (qString, findMoreAnswers(config, qString, ajs)) }
    // map back to Question objects
    val augmentedQuestions = moreAnswers.flatMap { case (qString, ajs) =>
      if (ajs.nonEmpty)
        ajs.iterator.map { case (answer, aj) => Question(qString, Some(aj)) }
      else 
        Iterator(Question(qString, None)) 
    }
    // sort new answers to the top of the list,
    // and unanswered questions to the bottom.
    augmentedQuestions.toSeq.sortBy { q =>
      q.answerJustification match {
        case Some(AnswerJustification("X", _, _, _)) => 0
        case Some(x: AnswerJustification) => 0.5
        case None => 1
      }
    }
  }
  
  def findMoreAnswers(config: Config, qString: String, answerJusts: Map[String, AnswerJustification]): Map[String, AnswerJustification] = {
    findTopAnswers(config, qString) ++ answerJusts
  }
  
  def findTopAnswers(config: Config, question: String): Map[String, AnswerJustification] = {
    val scoredAnswerGroups = config.system.answer(question)
    val top = scoredAnswerGroups.take(config.numAnswers)
    val answerJust = top.map { topAnswer => 
      val answer = topAnswer.alternates.head.head
      val topTuple = topAnswer.derivations.head.etuple.tuple
      val justification = TuplePrinter.printTuple(topTuple)
      val query = topAnswer.derivations.head.etuple.equery.uquery.toString
      (answer -> AnswerJustification("X", answer, justification, query))
    }
    answerJust.toMap
  }
}