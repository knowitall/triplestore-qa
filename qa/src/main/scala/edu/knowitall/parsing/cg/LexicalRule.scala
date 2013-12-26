package edu.knowitall.parsing.cg

import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.util.NlpUtils
import java.io.InputStream
import scala.io.Source
import edu.knowitall.util.ResourceUtils

case class LexicalRule(syntax: TaggerExtractor, semantics: CategoryPattern) extends TerminalRule {
  override def apply(interval: Interval, sent: Sentence with Chunked with Lemmatized) = {
    val span = NlpUtils.split(sent, interval.start, interval.end)
    semantics(syntax(span))
  }
  override def toString = syntax.patternName
}

object LexicalRule {
  def fromString(s: String) = {
    s.split(":=", 2) match {
      case Array(synStr, semStr) => {
        val syntax = TaggerExtractor.fromString(synStr)
        val semantics = CategoryPattern.fromString(semStr)
        LexicalRule(syntax, semantics)
      }
      case _ => throw new IllegalArgumentException(s"Invalid lexical rule string: $s")
    }
  }
  def fromStrings(strings: IndexedSeq[String]) = for {
    line <- strings
    if !line.trim.startsWith("#") && line.trim != ""
  } yield LexicalRule.fromString(line.trim())
  def fromInputStream(is: InputStream) = fromStrings(Source.fromInputStream(is).getLines.toIndexedSeq)
  def fromFile(path: String) = fromStrings(Source.fromFile(path).getLines.toIndexedSeq)
  def fromResource(path: String) = fromInputStream(ResourceUtils.resource(path))
}