package edu.knowitall.parsing.cg

import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.repr.sentence.Lemmatizer
import edu.knowitall.repr.sentence.Chunker
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.repr.sentence.Postagger
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.chunk.{Chunker => ToolChunker}
import edu.knowitall.util.DummyChunker


case class CgParser() {
  
  
  private val postagger = new StanfordPostagger
  private val chunker = DummyChunker(postagger)
  
  private def process(text: String): Sentence with Chunked with Lemmatized = {
    new Sentence(text) with Chunker with Lemmatizer {
      val chunker = CgParser.this.chunker
      val lemmatizer = MorphaStemmer
    }
  }
  
  
  val stupidTerminal = new TerminalRule[Sentence with Chunked with Lemmatized] {
    override def apply(interval: Interval, s: Sentence with Chunked with Lemmatized) = {
      val txt = s.tokens.map(_.string).slice(interval.head, interval.end).mkString(" ")
      if (txt == "france") {
        Some(Arg(UnquotedTLiteral("franny")))
      } else if (txt == "is the capital of") {
        val lv = TVariable("x")
        val rv = TVariable("y")
        val q = ListConjunctiveQuery.fromString("($x, cappy, $y)").get
        Some(Binary(lv, rv, q))
      } else if (txt == "which" || txt == "what") {
        Some(Identity)
      } else if (txt == "city") {
        val v = TVariable("x")
        val q = ListConjunctiveQuery.fromString("($x, type, city)").get
        Some(Unary(v, q))
      } else {
        None
      }
    }
  }
  
  val terminals = IndexedSeq(stupidTerminal)
  val combinators = IndexedSeq(RightApply, LeftApply, UnaryIntersect, UnaryIdentity)
  
  val tagger = TerminalTagger.fromFile("src/main/resources/edu/knowitall/parsing/cg/lexicon.txt")
  
  def parse(s: String) = {
    val sent = process(s)
    println(sent.tokens.map(_.toString).mkString(" "))
    println
    tagger.getRules(sent) foreach println
    println
    
    val n = sent.tokens.size
    val cky = new CKY(sent, n, tagger.getRules(sent), combinators)
    cky.parse
    cky.nodes
  }

}

object MyTest extends App {

  val p = CgParser()
  val foo = p.parse(args(0))
  println( foo.keys.maxBy(_.span.size))
  
}