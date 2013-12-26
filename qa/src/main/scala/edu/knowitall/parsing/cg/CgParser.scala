package edu.knowitall.parsing.cg

import com.typesafe.config.ConfigFactory
import java.io.FileInputStream
import java.io.File
import edu.knowitall.util.ResourceUtils
import edu.knowitall.util.NlpTools
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.execution.ConjunctiveQuery

case class ParsedQuestion(query: ConjunctiveQuery, derivation: Derivation)

case class CgParser(lexicon: IndexedSeq[LexicalRule] = CgParser.defaultLexicon, 
					combinators: IndexedSeq[Combinator] = CgParser.defaultCombinators,
					chunker: Chunker = NlpTools.dummyChunker,
					lemmatizer: Stemmer = NlpTools.stemmer) {
 
  private def process(s: String) = NlpTools.process(s, chunker, lemmatizer)
 
  private def getQuery(cat: Category) = cat match {
    case Unary(freeVar, query) => Some(query)
    case _ => None
  }
  
  def parse(s: String) = {
    val sent = process(s)
    val n = sent.tokens.size
    val cky = new CKY(sent, n, lexicon, combinators)
    cky.parse
    for {
      derivation <- cky.rootDerivations
      query <- derivation.category match {
        case u: Unary => Some(u.query)
        case _ => None
      }
    } yield ParsedQuestion(query, derivation)
  }

}

case object CgParser {
  val conf = ConfigFactory.load()
  val defaultCombinators = IndexedSeq(RightApply, LeftApply, UnaryIntersect,
		  							  UnaryIdentity)
  lazy val lexiconIn = if (conf.hasPath("parsing.cg.lexiconPath")) {
    new FileInputStream(new File(conf.getString("parsing.cg.lexiconPath")))
  } else {
    ResourceUtils.resource(conf.getString("parsing.cg.lexiconClasspath"))
  }
  lazy val defaultLexicon = LexicalRule.fromInputStream(lexiconIn)
  
}

object MyTest extends App {
  val parser = CgParser()
  val results = parser.parse(args(0))
  for (deriv <- results) {
    println(deriv.query)
    deriv.derivation.terminalRules foreach println
    println
  }
}
