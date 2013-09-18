package edu.knowitall.scoring.eval

import java.io.PrintStream	
import java.io.File
import scala.io.Source
import scopt.OptionParser
import edu.knowitall.common.Resource.using
import edu.knowitall.util.TuplePrinter

/**
 * Takes as input a system configuration and a labeled gold set. Runs the system on questions in the set,
 * matching top answers with labels in the gold set. Produces Precision/Yield data and diagnostic info.
 */
object SystemAnalyzer {

  import AnswerAnnotator.Config 
  import AnswerAnnotator.{Question, AnswerJustification}
  
  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("AnswerAnnotator") {
      arg[File]("inputFile")  action { (f, c) => c.copy(inputFile = f) }
      arg[File]("outputFile") action { (f, c) => c.copy(outputFile = f) }
      opt[Int]("numAnswers")  action { (f, c) => c.copy(numAnswers = f) } text("Number of new answers to add")
      opt[String]("parser")   action { (s, c) => c.copy(parser = s) }
      opt[String]("executor") action { (s, c) => c.copy(executor = s) }
      opt[String]("basic")    action { (s, c) => c.copy(grouper = s) }
      opt[String]("scorer")   action { (s, c) => c.copy(scorer = s) }
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

      run(config, questions, output)
    }
  }
  
  def run(config: Config, questions: Seq[Question], output: PrintStream): Unit = {
    
    val questionGroups = questions.groupBy(_.question)
    val qaJustGroups = questionGroups.map { case (question, qs) => 
      val ajMap = qs.flatMap(_.answerJustification).map(aj => (aj.answer, aj)).toMap
      (question, ajMap)
    }
    val systemAnswers = questionGroups.keysIterator.map({ question => 
      (question, findTopScoredAnswers(config, question))
    }).toMap
    
    // find labels for the SystemAnswers
    val labeledSystemAnswers = systemAnswers.iterator.map({ case (qString, ajMap) =>
      val goldAjs = qaJustGroups.getOrElse(qString, Map.empty)
      val labeledAjs = ajMap.iterator.flatMap({ case (answer, (_, score)) =>
        val goldAj = goldAjs.get(answer)
        goldAj match {
          case Some(AnswerJustification("0" | "1", _, _, _)) => Some((answer, (goldAj.get, score)))
          case _ => {
            System.err.println(s"Warning, no label for (Q,A): ($qString, $answer)")
            None
          }
        }
      }).toMap
      (qString, labeledAjs)
    }).toMap
    // flatten labeled question/answers
    val questionAnswers = labeledSystemAnswers.iterator.flatMap({ case (qString, ajMap) =>
      ajMap.iterator.map { case (answer, (aj, score)) => (Question(qString, Some(aj)), score) }  
    }).toSeq
    
    reportPerformance(config, output, questionAnswers, questions.size.toDouble)
  }
  
  def reportPerformance(config: Config, output: PrintStream, scoredQAs: Seq[(Question, Double)], totalQuestions: Double): Unit = {
    val sortedQAs = scoredQAs.sortBy(-_._2)
    
    output.println("RECALL\tPRECISION")
    val bools = sortedQAs.map(_._1.answerJustification.get.label == "1")
    val precs = precRecall(bools)
    val py = precs.zipWithIndex
    val pr = py.map { case (prec, yld) => (prec, (yld+ 1.0).toDouble/totalQuestions) }
    pr.foreach { case (prec, recall) => output.println(s"%.03f$recall\t%.03f$prec") }
    
    output.print("\n\n\n")
    
    sortedQAs.foreach { case (q, score) =>
      output.println(f"$score%.03f\t$q")  
    }
  }
  
  def findTopScoredAnswers(config: Config, question: String): Map[String, (AnswerJustification, Double)] = {
    val scoredAnswerGroups = config.system.answer(question)
    val top = scoredAnswerGroups.take(config.numAnswers)
    val answerJust = top.map { topAnswer => 
      val answer = topAnswer.alternates.head.head
      val topTuple = topAnswer.derivations.head.etuple.tuple
      val justification = TuplePrinter.printTuple(topTuple)
      val query = topAnswer.derivations.head.etuple.equery.uquery.toString
      (answer -> (AnswerJustification("X", answer, justification, query), topAnswer.score))
    }
    answerJust.toMap
  }
  
  def precRecall(sorted: Seq[Boolean]): Seq[Double] = {
    
    var result: List[Double] = Nil
    
    var total = 0
    var correct = 0
    
    for (label <- sorted) {
      total += 1
      if (label) {
        correct += 1
        result ::= (correct.toDouble / total.toDouble)
      }
    }
    result.reverse.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
  } 
}