package edu.knowitall.parsing.cg

import edu.knowitall.taggers.TaggerCollection
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.tool.typer.Type
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.taggers.ParseRule
import java.io.InputStream
import scala.io.Source
import java.io.FileInputStream
import java.io.File
import edu.knowitall.util.MathUtils
import edu.knowitall.util.NlpUtils
import edu.knowitall.taggers.NamedGroupType

case class TaggerRule(name: String, interval: Interval, category: Category)
extends TerminalRule[Sentence with Chunked with Lemmatized] {
  override def apply(i: Interval, s: Sentence with Chunked with Lemmatized) = 
    if (i == interval) {
      Some(category)
    } else {
      None
    }
}

case class TerminalTagger(tc: TaggerCollection[Sentence with Chunked with Lemmatized] = TerminalTagger.defaultTc) {
  
  private val binaryPrefix = "BinaryRule"
  private val isaPrefix = "IsaRule"
  private val argPrefix = "ArgRule"
  private val idPrefix = "IdentityRule"
  private val prefixPat = ("^(" + List(binaryPrefix, isaPrefix, argPrefix, idPrefix).mkString("|") + ").*\\.val$").r
  println(prefixPat)
  private val isaRel = "be a" 
    
  private def makeBinary(s: String) = {
    val x = TVariable("x")
    val r = TVariable("r")
    val y = TVariable("y")
    val q = ListConjunctiveQuery.fromString("($x, $r, $y)").get
    Binary(x, y, q.subs(r, UnquotedTLiteral(s)))
  }
  
  private def makeArg(s: String) = Arg(UnquotedTLiteral(s))
  
  private def makeIsa(s: String) =
    RightApply(makeBinary(isaRel), makeArg(s)).get
  
  private def makeIdentity(s: String) = Identity()
  
  private val makeMap = Map(binaryPrefix -> makeBinary _,
		  					isaPrefix -> makeIsa _,
		  					idPrefix -> makeIdentity _,
		  					argPrefix -> makeArg _)
  
  private def getPrefix(t: Type) = t.name match {
    case prefixPat(prefix) => Some(prefix)
    case _ => None
  }
  
  private def makeCategory(t: Type) = for {
    prefix <- getPrefix(t)
    function <- makeMap.get(prefix)
  } yield function(t.text)
  
  private def tag(s: Sentence with Chunked with Lemmatized) = for {
    t <- tc.tag(s)
    cat <- makeCategory(t)
    ruleName = t.name
    interval = t.tokenInterval
  } yield TaggerRule(ruleName, interval, cat)
  
  def getRules(s: Sentence with Chunked with Lemmatized) = for {
    interval <- MathUtils.allIntervals(s.tokens.size)
    subsent = NlpUtils.split(s, interval.start, interval.end)
    rule <- tag(subsent)    
    newRule = rule.copy(interval = rule.interval.shift(interval.start))
  } yield newRule
  
}

case object TerminalTagger {
  val defaultPath = "/edu/knowitall/parsing/cg/lexicon.txt"
  lazy val defaultTc = fromInputStream(getClass.getResourceAsStream(defaultPath)).tc
  def fromInputStream(in: InputStream) = {
    fromString(Source.fromInputStream(in, "UTF-8").getLines.mkString("\n"))
  }
  def fromFile(s: String) = fromInputStream(new FileInputStream(new File(s)))
  def fromString(s: String) = {
    val rules = new ParseRule[Sentence with Chunked with Lemmatized].parse(s).get
    val tc = rules.foldLeft(new TaggerCollection[Sentence with Chunked with Lemmatized]()){ case (ctc, rule) => ctc + rule }
    TerminalTagger(tc)
  }
}