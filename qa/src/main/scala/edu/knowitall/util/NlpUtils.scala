package edu.knowitall.util

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.chunk.Chunker

object NlpUtils {
  
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
  
  def sentFromFields(fields: Seq[String]): Option[Seq[Lemmatized[ChunkedToken]]] = {
    fields match {
      case stokens :: slemmas :: stags :: Nil => {
        val tokens = stokens.split("\t").toSeq.map(t => new Token(t, 0))
        val lemmas = slemmas.split("\t").toSeq
        val tags = stags.split("\t").toSeq
        val taggedTokens = (tokens zip tags) map { case (token, tag) => new PostaggedToken(token, tag) }
        val chunkedTokens = taggedTokens.map(t => new ChunkedToken(t, ""))
        val res = (chunkedTokens zip lemmas) map { case (tt, lem) => new Lemmatized(tt, lem) }
        Some(res)
      } 
      case _ => None
    }
  }

}