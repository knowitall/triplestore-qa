package edu.knowitall.parsing.cg

import edu.knowitall.taggers.tag.PatternTagger
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.execution.TVariable
import edu.knowitall.taggers.NamedGroupType
import edu.knowitall.tool.typer.Type
import scala.util.Try
import edu.knowitall.util.NlpTools

case class TaggerExtractor(tagger: PatternTagger) {
  val patternName = tagger.name
  private val variablePattern = (patternName + "\\.(.*)").r 
  private def nameToVar(t: Type) = t.name match {
    case variablePattern(v) => Some(TVariable(v))
    case _ => None
  }
  def extract(s: Sentence with Chunked with Lemmatized): Map[TVariable, String] = {
    val bindings = for {
      t <- tagger(s)
      variable <- nameToVar(t)
      value = t.text
    } yield (variable -> value)
    bindings.toMap
  }
  def apply(s: Sentence with Chunked with Lemmatized) = extract(s)
}

case object TaggerExtractor {
  private val defPattern = "^([A-Za-z]+[a-z0-9]*)\\s*(.*)$".r
  def fromString(s: String) = s.trim match {
    case defPattern(name, pattern) => TaggerExtractor(new PatternTagger(name, Seq("^"+pattern+"$")))
    case _ => throw new IllegalArgumentException("Invalid definition: $s")
  }
  def fromLines(lines: Iterable[String]) = lines map fromString 
}