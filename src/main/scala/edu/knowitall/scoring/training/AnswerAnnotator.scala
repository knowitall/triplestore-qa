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
class AnswerAnnotator(val input: Iterator[String], val output: PrintStream) {

  case class InputRecord(nlQuery: String, uQuery: String)
  object InputRecord {
    def fromString(str: String) = str.split("\t") match {
      case Array(nlq, uq, _*) => InputRecord(nlq, uq)
      case _ => throw new RuntimeException(s"Unrecognized input record: $str")
    }
  }

  case class OutputRecord(answer: String, uQuery: String, nlQuery: String) {
    override def toString = Seq(answer, uQuery, nlQuery).mkString("\t")
  }
  object OutputRecord {
    def fromGroup(input: InputRecord, group: AnswerGroup): OutputRecord = {
      OutputRecord(group.alternates.head.head, input.uQuery, input.nlQuery)
    }
  }
  
  import edu.knowitall.parsing.FormalQuestionParser
  import edu.knowitall.execution.{IdentityExecutor, BasicAnswerGrouper}
  import edu.knowitall.triplestore.{SolrClient, CachedTriplestoreClient}
  
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
    val sortedGroups = groups.sortBy(-_.derivations.size).take(10) 
    sortedGroups map { g => OutputRecord.fromGroup(input, g) }
  }
  
  def go(): Unit = {
    
    val answers = inputRecords flatMap answersFor
    
    answers foreach output.println
  }
}

object AnswerAnnotator extends App {
  
  import scopt.OptionParser
  import java.io.File
  import edu.knowitall.common.Resource.using
  
  case class Config(
      val inputQueries: File = new File("."),
      val output: PrintStream = System.out)
  
  val parser = new OptionParser[Config]("AnswerAnnotator") {
    arg[File]("inputQueries") action { (file, config) => config.copy(inputQueries = file) } text("Tab delimited file with columns: Natural Language Query | Formal Query")
    opt[File]("outFile") action { (file, config) => config.copy(output = new PrintStream(file)) } text("Optional output file, default stdout.")
  }
  
  parser.parse(args, Config()).foreach { config =>
    using(io.Source.fromFile(config.inputQueries)) { source =>
      new AnswerAnnotator(source.getLines, config.output).go()
    }
    if (config.output != System.out) config.output.close()
  }
}