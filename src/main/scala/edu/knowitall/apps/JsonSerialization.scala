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

object JsonSerialization {
  implicit val formats = DefaultFormats + MapSerializer + TValSerializer
  def serialize(any: Any): String = pretty(render(decompose(any)))
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