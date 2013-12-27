package edu.knowitall.parsing

import edu.knowitall.execution.Search.{arg1, arg2, rel}
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.tool.tokenize.{ClearTokenizer => Tokenizer}
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.TVal
import edu.knowitall.execution.Search.Conjunction
import edu.knowitall.execution.Search.FieldKeywords
import edu.knowitall.execution.TConjunct
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.ListConjunctiveQuery

trait QuestionParser {
  
  def parse(q: String): Iterable[ConjunctiveQuery]

}

case class FormalQuestionParser() extends QuestionParser {
  override def parse(q: String): List[ListConjunctiveQuery] = ListConjunctiveQuery.fromString(q) match {
    case Some(cq: ListConjunctiveQuery) => List(cq)
    case _ => List()
  }
}

abstract class LexiconParser extends QuestionParser {
  
  def lexicon: Lexicon
  
  val tokenizer = new Tokenizer
  override def parse(q: String): Iterable[ConjunctiveQuery] = {
    val parser = BottomUpParser(lexicon)
    val tokens = tokenizer.tokenize(q)
    val words = tokens.map(t => QWord(t.string)).toIndexedSeq
    val derivs = parser.parse(words)
    derivs.map(d => queryToConj(q, d.query))
  }
  
  def queryToConj(ques: String, q: SimpleQuery): ConjunctiveQuery = {
    val entLit = UnquotedTLiteral(q.entity)
    val relLit = UnquotedTLiteral(q.relation)
    val tvar = TVariable("x")
    val varField = q.queryField match {
      case Arg1 => arg1
      case _ => arg2
    }
    val entField = q.queryField match {
      case Arg1 => arg2
      case _ => arg1
    }
    val vals = Map[Field, TVal]((varField, tvar), (entField, entLit), (rel, relLit))
    val conj = TConjunct("r", vals)
    ListConjunctiveQuery(List(tvar), List(conj))
  }
  
}

case class StringMatchingParser(client: TriplestoreClient) extends LexiconParser {
  val lexicon = StringMatchLexicon(client)
}

case class OldParalexParser() extends LexiconParser {
  val lexicon = new SolrLexicon()
  override val tokenizer = new ClearTokenizer
  val stemmer = new MorphaStemmer
  override def parse(q: String) = {
    super.parse(preprocess(q)).map(postprocess(_))
  }

  def preprocess(q: String): String = {
    val nlpd = tokenizer.tokenize(q.toLowerCase()).map(stemmer.stemToken(_))
    nlpd.map(_.lemma).mkString(" ")
  }
  
  def postprocess(cq: ConjunctiveQuery): ConjunctiveQuery = {
    val newConjuncts = cq.conjuncts.map(fixConjunct)
    ListConjunctiveQuery(cq.qVars, newConjuncts)
  }
  
  def fixConjunct(c: TConjunct): TConjunct = {
    val newVals = for ((f, v) <- c.values) yield (f, fixTVal(v))
    TConjunct(c.name, newVals)
  }
  
  def fixTVal(tval: TVal): TVal = tval match {
    case l: UnquotedTLiteral => fixLiteral(l.value)
    case l: QuotedTLiteral => fixLiteral(l.value)
    case _ => tval
  }
  
  def fixLiteral(l: String): UnquotedTLiteral = {
    UnquotedTLiteral(l.dropRight(2).replaceFirst("^be-", "").replace('-', ' '))
  }
}