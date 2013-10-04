package edu.knowitall.wikianswers

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.chunk.ChunkedToken

case class Question(text: String, annotated: Seq[Lemmatized[ChunkedToken]])

case object Question {
  def fromFields(fields: Seq[String]): Option[Question] = {
    fields match {
      case text :: stokens :: slemmas :: stags :: Nil => {
        val tokens = stokens.split(" ").toSeq.map(t => new Token(t, 0))
        val lemmas = slemmas.split(" ").toSeq
        val tags = stags.split(" ").toSeq
        val taggedTokens = (tokens zip tags) map { case (token, tag) => new PostaggedToken(token, tag) }
        val chunkedTokens = taggedTokens.map(t => new ChunkedToken(t, ""))
        val res = (chunkedTokens zip lemmas) map { case (tt, lem) => new Lemmatized(tt, lem) }
        Some(Question(text, res))
      } 
      case _ => None
    }
  }
}

case class QuestionCluster(questions: Seq[Question])

case object QuestionCluster {
  def fromString(s: String): QuestionCluster = {
    val fields = s.split("\t").toList
    val grouped = fields.grouped(4).toList
    val questions = grouped.flatMap(Question.fromFields).toSeq
    QuestionCluster(questions)
  }
}