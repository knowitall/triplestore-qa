package edu.knowitall.parsing

import edu.knowitall.execution.UQuery
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

trait QuestionParser {
  
  def parse(q: String): Iterable[UQuery]

}

case class FormalQuestionParser() extends QuestionParser {
  override def parse(q: String) = ListConjunctiveQuery.fromString(q) match {
    case Some(cq: ListConjunctiveQuery) => ListConjunctiveQuery.expandSetTLiterals(cq)
    case _ => List()
  }
}

case class StringMatchingParser(client: TriplestoreClient) extends QuestionParser {
  
  val lexicon = StringMatchLexicon(client)
  val parser = BottomUpParser(lexicon)
  val tokenizer = new Tokenizer
  override def parse(q: String) = {
    val tokens = tokenizer.tokenize(q)
    val words = tokens.map(t => QWord(t.string)).toIndexedSeq
    val derivs = parser.parse(words)
    derivs.map(d => queryToConj(d.query))
  }
  
  def queryToConj(q: SimpleQuery): ConjunctiveQuery = {
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
    ListConjunctiveQuery(tvar, List(conj))
  }
  
}