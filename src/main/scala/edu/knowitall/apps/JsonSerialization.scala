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
import edu.knowitall.execution.ExecQuery
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import scala.language.reflectiveCalls
import edu.knowitall.execution.AnswerDerivation
import org.slf4j.LoggerFactory

object JsonSerialization {
  
  val logger = LoggerFactory.getLogger(this.getClass) 
  
  implicit val formats = DefaultFormats + MapSerializer + TValSerializer
  def serialize(any: Any): String = pretty(render(decompose(any)))
  def serializeAnswers(groups: List[ScoredAnswerGroup]): String = {
    val invs = groups.map(ScoredAnswerInv.fromScoredAnswerGroup(_))
    pretty(render(decompose(invs)))
  }
}

case class ScoredAnswerInv(answers: List[String], alternates: List[List[String]], 
    score: Double, uqueries: List[UQueryInv])
object ScoredAnswerInv {
  def fromScoredAnswerGroup(sa: ScoredAnswerGroup): ScoredAnswerInv = {
    val derivs = sa.derivations
    val uqs = UQueryInv.fromDerivs(derivs)
    val result = ScoredAnswerInv(sa.answer, sa.alternates, sa.score, uqs.toList)
    result
  }
}

case class UQueryInv(uquery: UQuery, equeries: List[ExecQueryInv])
object UQueryInv {
  def fromDerivs(ds: List[AnswerDerivation]): List[UQueryInv] = {
    val grpd = ds.groupBy(_.etuple.equery.uquery)
    val results = for ((uqr, derivs) <- grpd;
                       eqs = ExecQueryInv.fromDerivs(derivs)) 
                  yield UQueryInv(uqr, eqs.toList)
    results.toList
  }
}

case class ExecQueryInv(equery: ExecQuery, attrs: List[String], tuples: List[Map[String, Any]])
object ExecQueryInv {
  def fromDerivs(ds: List[AnswerDerivation]): List[ExecQueryInv] = {
    val grpd = ds.groupBy(_.etuple.equery)
    val tuples = for ((eqr, dsg) <- grpd; ts = dsg.map(_.etuple.tuple)) 
                 yield (eqr, ts.map(_.attrs))
    val attrs = for ((eqr, dsg) <- grpd; d <- dsg) yield (eqr, d.attrs)
    val results = for ((eqr, ts) <- tuples; as <- attrs.get(eqr)) 
                  yield ExecQueryInv(eqr, as, ts)
    results.toList
  }
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