package edu.knowitall.scoring.training

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.execution.BasicAnswerGrouper
import java.net.URL
import edu.knowitall.common.Resource.using
import scala.util.{Try, Success, Failure}

class TrainingDataReader(val trainingResource: URL) extends Iterable[Labelled[AnswerGroup]] {

  import TrainingDataReader.InputRecord
  
  val parser = new FormalQuestionParser()
  val baseclient = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val cachedClient = CachedTriplestoreClient(baseclient, 5000)
  val executor = new IdentityExecutor(cachedClient)
  val grouper = new BasicAnswerGrouper()
  
  type LAG = Labelled[AnswerGroup]
  
  def expectedGroupFilter(expectedAnswer: String)(group: AnswerGroup): Boolean = {
    val alternates = group.alternates.toSet
    alternates.contains(List(expectedAnswer))
  }
  
  def groupsForQuery(uQueryString: String) = {
    val uQueries = parser.parse(uQueryString)
    val answers = uQueries flatMap executor.deriveAnswers
    val allGroups = grouper group answers.toList
    allGroups
  }
  
  def labeledAnswerGroup(inputRec: InputRecord, groups: Seq[AnswerGroup]): Try[LAG] = {
    // run the uquery to get answergroups
    val allGroups = groupsForQuery(inputRec.uquery)
    // get just the groups with the answer we're interested in
    val groups = allGroups filter expectedGroupFilter(inputRec.answer)
    // take just the first one for now...
    Try {
      require(groups.size >= 1, s"No results obtained for training record $inputRec")
      Labelled(inputRec.label, groups.head)
    }
  }
  
  def inputRecords = using(io.Source.fromURL(trainingResource, "UTF8")) { source =>
    val inputRecs = source.getLines map { l => InputRecord.fromString(l) }
    inputRecs.toList
  }
  
  def queryGroupedRecs = inputRecords.groupBy(_.uquery)
  
  lazy val labeledAnswerGroups = {
    val queryRecsPairs = queryGroupedRecs.iterator.toSeq
    val groupsRecsPairs = queryRecsPairs.map { case (queryString, inputRecs) =>
      val groups = groupsForQuery(queryString)
      (groups, inputRecs)
    }
    groupsRecsPairs.flatMap { case (groups, inputRecs) =>
      val lagTrys = inputRecs.map { inputRec => labeledAnswerGroup(inputRec, groups) }
      lagTrys flatMap {
        case Success(lag) => Some(lag)
        case Failure(e) => { System.err.println(e.getMessage); None }
      }
    }
  }
  
  override def iterator: Iterator[LAG] = labeledAnswerGroups.iterator
}

object TrainingDataReader {
  
  case class InputRecord(val label: Boolean, val answer: String, val uquery: String, val nlQuery: String)

  object InputRecord {
    def fromString(str: String) = str.split("\t") match {
      case Array(lString, answer, uqString, nlqString) => {
        val label = lString match {
          case "0" => false
          case "1" => true
          case _ => throw new RuntimeException(s"Invalid label: $lString")
        }
        InputRecord(label, answer, uqString, nlqString)
      }
    }
  }
    
  def trainingResource = {
    val url = getClass.getResource("scorer-training.txt")
    require(url != null, "Could not find resource: scorer-training.txt")
    url
  }
    
  lazy val defaultTraining: Seq[Labelled[AnswerGroup]] = new TrainingDataReader(trainingResource).toSeq
}