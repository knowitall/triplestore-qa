package edu.knowitall.apps

import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import net.liftweb.json.Serializer
import net.liftweb.json.Formats
import net.liftweb.json.Extraction
import net.liftweb.json.TypeInfo
import edu.knowitall.execution.TVal
import net.liftweb.json.DefaultFormats
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import scala.language.reflectiveCalls
import org.slf4j.LoggerFactory
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ConjunctTable
import edu.knowitall.execution.InvertedAnswerDerivation

object JsonSerialization {
  
  val logger = LoggerFactory.getLogger(this.getClass) 
  
  implicit val formats = DefaultFormats + MapSerializer + TValSerializer
  def serialize(any: Any): String = pretty(render(decompose(any)))
  def serializeAnswers(groups: List[ScoredAnswerGroup]): String = {
    val objs = groups.map(new SerializedAnswerGroup(_))
    pretty(render(decompose(objs)))
  }
}

case class SerializedAnswerGroup(answer: String, 
								 score: Double,
								 derivations: List[SerializedDerivation]) {
  def this(g: ScoredAnswerGroup) =
    this(g.answer.mkString(" "), g.score, g.invertedDerivations.map(new SerializedDerivation(_)))
}

case class SerializedDerivation(executedQuery: ConjunctiveQuery,
								paraphrases: List[String],
								parserQueries: List[ConjunctiveQuery],
								tables: List[ConjunctTable]) {
  def this(d: InvertedAnswerDerivation) =
    this(d.execQuery, d.paraphrases.map(_.target), d.parsedQueries, d.tables)
}

object MapSerializer extends Serializer[Map[Any, Any]] {
  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case m: Map[_, _] => JObject(m.map({
      case (k, v) => JField(
        k match {
          case ks: String => ks
          case ks: Symbol => ks.name
          case ks: Any => ks.toString
        },
        Extraction.decompose(v)
      )
    }).toList)
  }

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), 
    Map[Any, Any]] = {
    sys.error("Not interested.")
  }
}

object TValSerializer extends Serializer[TVal] {
  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case t: TVal => JString(t.toString())
  }
  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), 
    TVal] = {
    sys.error("Not interested.")
  }
}