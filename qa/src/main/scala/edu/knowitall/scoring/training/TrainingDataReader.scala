package edu.knowitall.scoring.training

import edu.knowitall.apps.QAConfig
import edu.knowitall.apps.QASystem
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.paralex.ParalexQuestionParser
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

  type LAG = Labelled[AnswerGroup]

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