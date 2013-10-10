package edu.knowitall.scoring.eval

import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.scoring.AnswerScorer
import org.apache.commons.codec.binary.Base64
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

class SystemAnalyzer(config: Config, results: Seq[InputRecord]) {

  val rescored = {
    if (config.system.ranker.isInstanceOf[AnswerScorer]) {
      results.filter(r => r.json.isDefined && r.label.isDefined).map { r =>
        val sag = InputRecord.b64deserializeSag(r.json.get)
        val score = reScoreSag(r.question, sag)
        r.copy(scoreString = Some("%.03f" format score))
      }
    } else results
  }

  // enforce max numAnswers per question by taking the top scored answers.
  val numLimited = {
    val questionResultsGroups = rescored.groupBy(_.question).valuesIterator
    val limitedGroups = questionResultsGroups.map(_.sortBy(-_.scoreString.get.toDouble).take(config.numAnswers))
    limitedGroups.toSeq.flatten
  }

  val sorted = numLimited.sortBy(-_.scoreString.get.toDouble).map(_.copy(json = None))

  def reScoreSag(originalQuestion: String, sag: ScoredAnswerGroup): Double = {
    config.system.ranker.asInstanceOf[AnswerScorer].scoreAnswer(originalQuestion, sag).score
  }

  def precRecall = {
    val booleans = sorted.map(_.label.get.trim == "1")
    precRecallHelper(booleans)
  }

  def precRecallHelper(sorted: Seq[Boolean]): Seq[Double] = {

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
    result.reverse//.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
  }
}

object SystemAnalyzer {
  import scopt.OptionParser
  import edu.knowitall.common.Resource.using
  import java.io.File

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("SystemAnalyzer") {
      arg[File]("inputFile")  action { (f, c) => c.copy(inputFile = f) }
      arg[File]("outputFile") action { (f, c) => c.copy(outputFile = f) }
      opt[Int]("numAnswers")  action { (f, c) => c.copy(numAnswers = f) } text("Number of top answers to consider")
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

    using(io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      using(new java.io.PrintStream(config.outputFile, "UTF8")) { outStream =>

        val inputRecords = source.getLines.map(InputRecord.fromString).toSeq

        def filter(rec: InputRecord) = {
          rec.parser == config.parser &&
            rec.executor == config.executor &&
            rec.grouper == config.grouper &&
            rec.scorer == config.scorer
        }

        val filteredRecords = inputRecords.filter(filter)

        val analyzer = new SystemAnalyzer(config, filteredRecords)

        outStream.println(Seq(config.parser, config.executor, config.grouper, config.scorer).mkString(", "))
        analyzer.precRecall foreach outStream.println
        outStream.println()
        outStream.println(InputRecord.headers)
        analyzer.sorted foreach outStream.println
      }
    }
  }
}