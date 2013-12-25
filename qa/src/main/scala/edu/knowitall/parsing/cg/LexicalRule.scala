package edu.knowitall.parsing.cg

import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.util.NlpUtils

case class LexicalRule(syntax: TaggerExtractor, semantics: CategoryPattern) extends TerminalRule[Sentence with Chunked with Lemmatized] {
  override def apply(interval: Interval, sent: Sentence with Chunked with Lemmatized) = {
    val span = NlpUtils.split(sent, interval.start, interval.end)
    semantics(syntax(span))
  }
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
}