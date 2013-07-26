package edu.knowitall.scoring.training

import java.io.PrintStream
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery

/**
 * Reads in (nat. lang. query, uquery) as text, finds answers for uquery, and 
 * outputs (answer, uquery, nat. lang. query). One answer per line. For labeling. 
 * 
 * Output is labeled by a human and then fed into TrainingDataReader, which
 * provides an Iterable of labeled answergroups for a training function.
 */
class AnswerAnnotator(val input: Iterator[String]) {

  import edu.knowitall.parsing.FormalQuestionParser
  import edu.knowitall.execution.{IdentityExecutor, BasicAnswerGrouper}
  import edu.knowitall.triplestore.{SolrClient, CachedTriplestoreClient}
  import AnswerAnnotator.{InputRecord, OutputRecord}
  
  val parser = new FormalQuestionParser()
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 100000)
  val executor = new IdentityExecutor(client)
  val grouper = new BasicAnswerGrouper()
  
  def inputRecords = input filter(_.nonEmpty) map InputRecord.fromString
  
  def answersFor(input: InputRecord): Seq[OutputRecord] = {
    
    val uQueries = parser.parse(input.uQuery)
    val answers = uQueries flatMap executor.deriveAnswers
    val groups = grouper group answers.toList
    val sortedGroups = groups.sortBy(-_.derivations.size).take(25) 
    sortedGroups map { g => OutputRecord.fromGroup(input, g) }
  }
  
  def outputRecords: Iterator[OutputRecord] = {
    
    inputRecords flatMap answersFor
  }
}

object AnswerAnnotator extends App {

  import scopt.OptionParser
  import java.io.File
  import edu.knowitall.common.Resource.using

  case class InputRecord(nlQuery: String, uQuery: String)
  object InputRecord {
    val splitRegex = "\t".r
    def fromString(str: String) = splitRegex.split(str) match {
      case Array(nlq, uq, _*) => InputRecord(nlq, uq)
      case _ => throw new RuntimeException(s"Unrecognized input record: $str")
    }
  }

  case class OutputRecord(answer: String, uQuery: String, nlQuery: String) {
    override def toString = Seq(answer, uQuery, nlQuery).mkString("\t")
  }
  object OutputRecord {
    import InputRecord.splitRegex
    def fromGroup(input: InputRecord, group: AnswerGroup): OutputRecord = {
      OutputRecord(group.alternates.head.head, input.uQuery, input.nlQuery)
    }
    def fromString(s: String) = {
      val splits = splitRegex.split(s).map(_.trim)
      splits match {
        case Array(answer, uQuery, nlQuery) => OutputRecord(answer, uQuery, nlQuery)
        case a => throw new RuntimeException(s"Invalid OutputRecord format: ${a.mkString(" ")}")
      }
    }
  }

  case class Config(
    val inputQueries: File = new File("."),
    val output: PrintStream = System.out)

  val parser = new OptionParser[Config]("AnswerAnnotator") {
    arg[File]("inputQueries") action { (file, config) => config.copy(inputQueries = file) } text ("Tab delimited file with columns: Natural Language Query | Formal Query")
    opt[File]("outFile") action { (file, config) => config.copy(output = new PrintStream(file)) } text ("Optional output file, default stdout.")
  }

  parser.parse(args, Config()).foreach { config =>
    using(io.Source.fromFile(config.inputQueries)) { source =>
      val annotator = new AnswerAnnotator(source.getLines)
      val outputRecords = annotator.outputRecords
      outputRecords foreach config.output.println
    }
    if (config.output != System.out) config.output.close()
  }
}