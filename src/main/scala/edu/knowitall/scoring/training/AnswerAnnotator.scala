package edu.knowitall.scoring.training

import java.io.PrintStream
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery

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
    OutputRecord(group.answer, input.uQuery, input.nlQuery)
  }
}

/**
 * Reads in (nat. lang. query, uquery) as text, finds answers for uquery, and 
 * outputs (answer, uquery, nat. lang. query). One answer per line. For labelling. 
 */
class AnswerAnnotator(val input: Iterable[String], val output: PrintStream) {

  import edu.knowitall.parsing.FormalQuestionParser
  import edu.knowitall.execution.{IdentityExecutor, BasicAnswerGrouper}
  import edu.knowitall.triplestore.{SolrClient, CachedTriplestoreClient}
  
  val parser = new FormalQuestionParser()
  val baseClient = SolrClient("http://rv-n12:8983/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 100000)
  val executor = new IdentityExecutor(client)
  val grouper = new BasicAnswerGrouper()
  
  def inputRecords = input map InputRecord.fromString
  
  def answersFor(input: InputRecord): Seq[OutputRecord] = {
    
    val uQueries = parser.parse(input.uQuery)
    val answers = uQueries flatMap executor.deriveAnswers
    val groups = grouper group answers.toList
    groups map { g => OutputRecord.fromGroup(input, g) }
  }
  
  def go(): Unit = {
    
    val answers = inputRecords flatMap answersFor
    
    answers foreach output.println
  }
}