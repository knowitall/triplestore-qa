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
import edu.knowitall.scoring.eval.{InputRecord => EvalRecord}

class TrainingDataReader(val trainingResource: URL) extends Iterable[Labelled[AnswerGroup]] {

  val parser = new FormalQuestionParser()
  val baseclient = SolrClient("http://rv-n12.cs.washington.edu:10893/solr/triplestore", 500)
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

  def labeledAnswerGroups = using(io.Source.fromURL(trainingResource, "UTF8")) { source =>
    val inputRecs = source.getLines map { l => EvalRecord.fromString(l) }
    inputRecs.flatMap(toLabeledScoredAnswerGroup).toList
  }

  def toLabeledScoredAnswerGroup(ir: EvalRecord): Option[LAG] = {
    if (ir.label.isDefined && ir.json.isDefined) {
      val label = ir.label.get.trim == "1"
      val ag = EvalRecord.b64deserializeSag(ir.json.get)
      Some(Labelled(label, ag))
    } else None
  }

  override def iterator: Iterator[LAG] = labeledAnswerGroups.iterator
}

object TrainingDataReader {

  def trainingResource = {
    val url = getClass.getResource("scorer-training.txt")
    require(url != null, "Could not find resource: scorer-training.txt")
    url
  }

  val defaultTraining: Seq[Labelled[AnswerGroup]] = new TrainingDataReader(trainingResource).toSeq
}