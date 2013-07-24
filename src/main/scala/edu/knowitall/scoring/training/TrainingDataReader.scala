package edu.knowitall.scoring.training

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.execution.BasicAnswerGrouper
import java.net.URL
import scala.util.{Try, Success, Failure}

class TrainingDataReader(val trainingResource: URL) extends Iterable[Labelled[AnswerGroup]] {
  
  val parser = new FormalQuestionParser()
  val client = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val executor = new IdentityExecutor(client)
  val grouper = new BasicAnswerGrouper()
  
  type LAG = Labelled[AnswerGroup]
  
  case class InputRecord(val label: Boolean, val answer: String, val uquery: String, val nlQuery: String)
  
  def readInputRecord(str: String) = str.split("\t") match {
    case Array(lString, answer, uqString, nlqString) => {
      val label = lString match {
        case "0" => false
        case "1" => true
        case _ => throw new RuntimeException(s"Invalid label: $lString")
      }
      InputRecord(label, answer, uqString, nlqString)
    }
  }
  
  def expectedGroupFilter(expectedAnswer: String)(group: AnswerGroup): Boolean = {
    val alternates = group.alternates.toSet
    alternates.contains(List(expectedAnswer))
  }
  
  def labeledAnswerGroup(inputRec: InputRecord): Try[LAG] = {
    // run the uquery to get answergroups
    val uQueries = parser.parse(inputRec.uquery)
    val answers = uQueries flatMap executor.deriveAnswers
    val allGroups = grouper group answers.toList
    // get just the groups with the answer we're interested in
    val groups = allGroups filter expectedGroupFilter(inputRec.answer)
    // take just the first one for now...
    Try {
      require(groups.size >= 1, s"No results obtained for training record $inputRec")
      Labelled(inputRec.label, groups.head)
    }
  }
  
  override def iterator: Iterator[LAG] = new Iterator[LAG]() {
    val source = io.Source.fromURL(trainingResource)
    val lines = source.getLines
    val inputRecs = lines map { l => readInputRecord(l) }
    val lags = inputRecs map labeledAnswerGroup flatMap {
      case Success(lag) => Some(lag)
      case Failure(e) => {
        System.err.println(e.getMessage())
        None
      }
    }
     
    var closed = false
    
    override def hasNext = {
      if (closed) false
      else if (!lags.hasNext) {
        source.close()
        closed = true
        false
      } else {
        true
      }
    }
    
    override def next = lags.next
  }
}