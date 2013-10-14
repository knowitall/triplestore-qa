package edu.knowitall.scoring.eval

import java.io.File
import edu.knowitall.apps.QAConfig
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.util.TuplePrinter
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import org.apache.commons.codec.binary.Base64

case class Config(
    inputFile: File = new File("."),
    outputFile: File = new File("."),
    numAnswers: Int = 1,
    removeStale: Boolean = false,
    paraphraser: String = "paraphrase",
    parser: String   = "regex",
    executor: String = "identity",
    grouper: String  = "basic",
    scorer: String   = "logistic") {

	def sysConfig = QAConfig(paraphraser, parser, executor, grouper, scorer)
    def system = {
	  QASystem.getInstance(sysConfig).get
	}
  }

/**
  * Only re-uses answer labels if the answer, question, and justification are exactly the same.
  */
class AnswerAnnotator(config: Config, input: Seq[InputRecord]) {

  def output(): Seq[InputRecord] = {

    // get map of (question string -> inputRecord)
    val questionMap = input.groupBy(_.question)
    // execute each question string under the given config,
    // and get map of question string -> input record
    // for the current execution
    val currentExecutionMap = execute(questionMap.keys)
    // merge the two -- In particular, results from currentExecutionMap will have no labels.
    // go through and find cases where the label might be re-used from previously existing results,
    // (the results in questionMap).
    val mergedMap = merge(questionMap, currentExecutionMap)

    // for convenience, sort the results so that unlabeled ones are at the very top, followed by
    // unlabeled ones, followed by parsed-but-unanswered ones, followed by the rest.
    convenienceSort(mergedMap.values.toSeq.flatten)
  }

  /**
   * sort the results so that unlabeled ones are at the very top, followed by
   * unlabeled ones, followed by parsed-but-unanswered ones, followed by the rest.
   */
  def convenienceSort(results: Seq[InputRecord]): Seq[InputRecord] = {
    val sortScores = results.map { r =>
      val sortScore = {
        if (r.answer.isDefined && !r.label.isDefined) 3
        else if (r.answer.isDefined) 2
        else if (r.query.isDefined) 1
        else 0
      }
      (r, sortScore)
    }
    sortScores.sortBy(-_._2).map(_._1)
  }

  /**
   * For results in newResultsMap that share the same answer, question, and justification
   * as a result in previousResultsMap, give the same label to the new result.
   *
   * For results in newResultsMap that share the same answer, question, justification, AND system configuration
   * use the new result's confidence value and paraphrase.
   *
   * Only keep the dummy InputRecords (with parse=None) if there are no other results with a parse.
   */
  def merge(previousResultsMap: Map[String, Seq[InputRecord]], newResultsMap: Map[String, Seq[InputRecord]]): Map[String, Seq[InputRecord]] = {

    val allKeys = (previousResultsMap.keySet ++ newResultsMap.keySet).toSeq
    allKeys.map { key =>
      val oldResults = previousResultsMap.get(key).toSeq.flatten
      val newResults = newResultsMap.get(key).toSeq.flatten
      (key, mergeSingleQuestionResults(oldResults, newResults))
    }.toMap
  }

  /**
   * Given a set of results for a particular question,
   * re-use labels where answers share the same answer string, justification string, and question string.
   * Then, if old and new results share all fields except confidence, update to new confidence.
   */
  def mergeSingleQuestionResults(oldResults: Seq[InputRecord], newResults: Seq[InputRecord]): Seq[InputRecord] = {

    val oldAJs = oldResults.groupBy(ir => (ir.answer, ir.just))
    val newAJs = newResults.groupBy(ir => (ir.answer, ir.just))
    val labelsReused = newAJs.map {
      case (key @ (Some(answer), Some(just)), results) => {
        // get old results for this (answer, just)
        val oldRs = oldAJs.get(key).toSeq.flatten
        // ensure that they all have the same label
        val oldLabels = oldRs.flatMap(_.label).distinct
        require(oldLabels.size <= 1, "All should have same label:\n"+oldRs.foreach(println))
        // assign the label to the new results.
        val labeledNewResults = results.map(_.copy(label = oldLabels.headOption))
        (key, labeledNewResults)
      }
      case kv => kv
    }
    // now, group them by everything except their score, and where groups have more than one score,
    // keep only the most recent.
    // also, now that we've re-used past labels, it's time to filter out old results from this
    // system config if the stale option is set.
    val filtered = if (config.removeStale) oldResults.filterNot { r =>
      r.parser == config.parser &&
      r.executor == config.executor &&
      r.grouper == config.grouper &&
      r.scorer == config.scorer
    } else oldResults
    val oldGroups = filtered.groupBy(_.copy(scoreString = None, json=None))
    val newGroups = labelsReused.values.toSeq.flatten.groupBy(_.copy(scoreString = None, json=None))
    val allKeys = (oldGroups.keySet ++ newGroups.keySet).toSeq
    val merged = allKeys.map(key => (oldGroups.get(key), newGroups.get(key))).map {
      case (_, Some(newR)) => newR.head
      case (Some(oldR), None) => oldR.head
      case (None, None) => throw new RuntimeException("This shouldn't happen!")
    }
    // finally, if merged has size greater than one, remove any dummy inputrecords
    if (merged.size > 1) merged.filterNot(_.query.isEmpty)
    else merged
  }


  /**
   * Paralex might crash due to KenLM server... so retry indefinitely. (hack)
   */
  def parseHelper(q: String): Iterable[ConjunctiveQuery] = {

    var done = false
    var queries = Iterable.empty[ConjunctiveQuery]
    var errors = 0
    var delayMs = 10

    while (!done) {
      try {
        queries = config.system.parser.parse(q)
        done = true
      } catch {
        case e: Exception => {
          errors += 1
          System.err.println(s"retry, sleep $delayMs ms #$errors: ${e.getMessage}")
          Thread.sleep(delayMs)
          if (delayMs < 500) delayMs *= 2
        }
      }
    }
    queries
  }

  /**
   * Execute each of the questions using the given config, returning a map
   * to the results obtained (in the form of inputrecords). Each question
   * shall have no more than config.numAnswers inputrecords associated
   * with it.
   */
  def execute(questions: Iterable[String]): Map[String, Seq[InputRecord]] = {

    val rawParses = questions.map { q =>
      val pps = config.system.paraphrase(q)
      val parses = pps.flatMap(pp => config.system.parse(pp).map(qq => (pp, qq)))
      if (parses.isEmpty) None
      else Some(parses.minBy(_._1.derivation.score))
    }

    val rawParsesAnswers = questions.map(q => (q, config.system.answer(q))).zip(rawParses).map { case ((question, answers), pparse) =>
      (question, answers, pparse.map(_._1), pparse.map(_._2))
    }

    val convertedAnswers = for ((question, sags, ppOpt, parseOpt) <- rawParsesAnswers;
                                  if (parseOpt.isDefined)) yield {
      if (!sags.isEmpty) (question, sags.map(s => convertAnswer(s, question)))
      else {
        val result = InputRecord(None,
          None,
          None,
          None,
          Some(parseOpt.get.toString),
          question,
          ppOpt.get.target,
          config.parser,
          config.executor,
          config.grouper,
          config.scorer,
          None)
        (question, Seq(result))
      }
    }
    convertedAnswers.toMap
  }

  private val cleanup = "\n+|\\s+".r

  def convertAnswer(sag: ScoredAnswerGroup, question: String): InputRecord = {
    val answer = sag.alternates.head.head
    val topTuple = sag.derivations.head.execTuple.tuple
    val justification = TuplePrinter.printTuple(topTuple)
    val query = sag.derivations.head.execTuple.query.toString
    val scoreString = "%.03f" format sag.score
    val paraphrase = sag.derivations.head.question

    val b64 = b64serializeSag(sag)

    InputRecord(None,
        Some(answer),
        Some(scoreString),
        Some(justification),
        Some(query),
        paraphrase,
        question,
        config.parser,
        config.executor,
        config.grouper,
        config.scorer,
        Some(b64))
  }

  private val b64 = new Base64()

  def b64serializeSag(sag: ScoredAnswerGroup): String = {
    val bos = new ByteArrayOutputStream();
    val out = new ObjectOutputStream(bos);
    out.writeObject(sag);
    val yourBytes = bos.toByteArray();
    out.close()
    bos.close()
    b64.encodeAsString(yourBytes)
  }


  def b64deserializeSag(string: String): ScoredAnswerGroup = {
    val yourBytes = new Base64().decode(string)
    val bis = new ByteArrayInputStream(yourBytes)
    val ois = new ObjectInputStream(bis)
    val sag = ois.readObject().asInstanceOf[ScoredAnswerGroup]
    bis.close()
    ois.close()
    sag
  }
}



object AnswerAnnotator {

  import scopt.OptionParser
  import edu.knowitall.common.Resource.using

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("AnswerAnnotator") {
      arg[File]("inputFile")  action { (f, c) => c.copy(inputFile = f) }
      arg[File]("outputFile") action { (f, c) => c.copy(outputFile = f) }
      opt[Int]("numAnswers")  action { (f, c) => c.copy(numAnswers = f) } text("Number of new answers to add")
      opt[Unit]("removeStale")  action { (f, c) => c.copy(removeStale = true) } text("Dont keep answers only found with this system config during prior runs")
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

    using (io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      using (new java.io.PrintStream(config.outputFile, "UTF8")) { output =>
        val inputRecords = source.getLines.map(InputRecord.fromString).toSeq
        val annotator = new AnswerAnnotator(config, inputRecords)
        annotator.output() foreach output.println
      }
    }
  }
}