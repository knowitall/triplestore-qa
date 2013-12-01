package edu.knowitall.util

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.chunk.Chunker
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.washington.cs.knowitall.logic.Expression.{Arg => LogicArg}
import edu.washington.cs.knowitall.regex.Expression.BaseExpression
import edu.washington.cs.knowitall.logic.LogicExpression
import com.google.common.base.{Function => GuavaFunction}

object NlpUtils {
  
  val months = Set("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  val days = Set("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
  val year = """^\d\d\d\d$""".r
  def isDateWord(s: String): Boolean = {
    months.contains(s) || days.contains(s) || { s match {
      case year() => true
      case _ => false
    }}
  }
  def isDate(s: String): Boolean = {
    s.split(" ").map(_.toLowerCase()).exists(isDateWord)
  }
  val qPrefixes = List("what year", "what month", "what day", "who", "when", "why", "what", "when", "where", "how", "be")
  def questionPrefix(s: String): String = {
    val x = s.toLowerCase()
    qPrefixes.find(prefix => x.startsWith(prefix)) match {
      case Some(prefix) => prefix
      case _ => "UNK"
    }
  }
  
  val lightVerbs = Set("be", "is", "are", "was", "were", "have", "has", "had")
  def isLightVerb(s: String) = lightVerbs contains s.toLowerCase()
  
  def serialize(sent: Seq[Lemmatized[ChunkedToken]]) = {
    val tokens = sent.map(l => l.token.string)
    val lemmas = sent.map(l => l.lemma)
    val tags = sent.map(l => l.postag)
    val chunks = sent.map(l => l.chunk)
    List(tokens, lemmas, tags, chunks).transpose.flatten.mkString(" ")
  }
  
  def deserialize(s: String): Seq[Lemmatized[ChunkedToken]]= {
    s.split(" ").grouped(4).toList.transpose match {
      case tokens :: lemmas :: tags :: chunks :: Nil => {
        val chunkedTokens = Chunker.tokensFrom(chunks, tags, tokens.map(t => new Token(t, 0)))
        (chunkedTokens zip lemmas) map {
          case (token, lemma) => new Lemmatized(token, lemma)
        }
      }
      case _ => throw new IllegalArgumentException(s"Could not deserialize: '$s'")
    }
  }
  
  type TokenType = Lemmatized[ChunkedToken]
  def makeRegex(expr: String): RegularExpression[TokenType] = {
    val factory = new ExpressionFactory[TokenType]() {
      override def create(token: String): BaseExpression[TokenType] = {
        new BaseExpression[TokenType](token) {
          val logic = LogicExpression.compile(token,
          new GuavaFunction[String, LogicArg[TokenType]]() {
            override def apply(s: String): LogicArg[TokenType] = {
              new LogicArg[TokenType]() {
                val pat = "(.*?)\\s*=\\s*'(.*)'".r
                override def apply(t: TokenType) = s match {
                  case pat("pos", value) => t.postag == value
                  case pat("lemma", value) => t.lemma.toLowerCase() == value.toLowerCase()
                  case pat("string", value) => t.string == value
                  case pat("chunk", value) => t.chunk == value
                }
              }
            }
          })
          override def apply(t: TokenType) = logic.apply(t)
        }
      }
    }
    RegularExpression.compile(expr, factory)
  }

}