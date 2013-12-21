package edu.knowitall.parsing.regex

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.Match
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.util.NlpUtils.makeRegex
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TConjunct
import scala.collection.JavaConversions._
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.util.NlpTools

case class RegexQuestionParser(
    chunker: Chunker = NlpTools.chunker,
    postagger: Postagger = NlpTools.tagger,
    patterns: Seq[RegexQuestionPattern] = RegexQuestionParser.defaultPatterns
    ) extends QuestionParser {

  def lemmatize(string: String): Seq[Lemmatized[ChunkedToken]] = chunker.synchronized {
    // use stanford postags if possible because they are less noisy...
    val chunks = chunker(string).toList
    val postags = if (string == "") Seq.empty else postagger(string)
    val repostagged = if (chunks.length != postags.length) chunks else {
      chunks.zip(postags).map { case (chunk, postag) =>
        new ChunkedToken(chunkSymbol=chunk.chunkSymbol, postagSymbol=postag.postagSymbol, string=postag.string, offset = postag.offset)}
    }
    val lemmas = repostagged map NlpTools.stemmer.stemPostaggedToken
    lemmas
  }

  def parse(question: String) = if (question.trim() == "") {
    Seq.empty
  } else {
    parse(lemmatize(question))
  }

  def parse(question: Seq[Lemmatized[ChunkedToken]]) = patterns.flatMap(p => p.parse(question))
  
  def parseWithPattern(question: String) = {
    val lemmas = lemmatize(question)
    for {
      p <- patterns
      q <- p.parse(lemmas)
    } yield (p, q)
  }
  
}

case object RegexQuestionParser {
  lazy val defaultPatterns = RegexQuestionPatterns.patterns
}