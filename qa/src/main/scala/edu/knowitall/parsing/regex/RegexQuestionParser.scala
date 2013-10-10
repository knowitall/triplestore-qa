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
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TConjunct
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.OpenNlpChunker
import scala.collection.JavaConversions._
import edu.knowitall.tool.postag.StanfordPostagger

case class RegexQuestionParser() extends QuestionParser {

  import RegexQuestionPatterns.patterns

  private val chunker = new OpenNlpChunker()
  private val postagger = new StanfordPostagger()

  def lemmatize(string: String): Seq[Lemmatized[ChunkedToken]] = chunker.synchronized {
    // use stanford postags if possible because they are less noisy...
    val chunks = chunker(string).toList
    val postags = postagger(string)
    val repostagged = if (chunks.length != postags.length) chunks else {
      chunks.zip(postags).map { case (chunk, postag) =>
        new ChunkedToken(chunk=chunk.chunk, postag=postag.postag, string=postag.string, offset = postag.offset)}
    }
    val lemmas = repostagged map MorphaStemmer.stemPostaggedToken
    lemmas
  }

  def parse(question: String) = parse(lemmatize(question)).map(uq => uq.copy(question = question))

  def parse(question: Seq[Lemmatized[ChunkedToken]]) = patterns.flatMap(p => p.parse(question))
}

object RegexQuestionParserRepl extends App {

  lazy val parser = new RegexQuestionParser

  io.Source.stdin.getLines.foreach { q =>
    val lemmasQ: Seq[Lemmatized[ChunkedToken]] = parser.lemmatize(q)
//    val strs = Seq("What", "studies", "all", "forms", "of", "life", "?")
//    val lems = Seq("What", "study", "all", "form", "of", "life", "?")
//    val postags = Seq("WDT", "VBZ", "DT", "NNS", "IN", "NN", ".")
//    val chunks = Seq("B-NP", "B-VP", "B-NP", "I-NP", "B-PP", "B-NP", "O")
//    val offsets = Seq(0, 4, 12, 16, 22, 25, 29)
//    val lemmas = strs.zip(postags).zip(chunks).zip(offsets).zip(lems).map {
//      case ((((str, postag), chunk), offset), lemma) =>
//        val chunkToken = new ChunkedToken(chunk=chunk, string = str, postag = postag, offset = offset)
//        Lemmatized(chunkToken, lemma)
//    }
//
//    println(lemmas)
    println(lemmasQ.map(_.postag))
    println(lemmasQ.map(_.chunk))
    val queries = RegexQuestionPatterns.patterns.map(_.parse(lemmasQ)).zipWithIndex
    queries.foreach { query =>
      println(query)
    }
  }
}
