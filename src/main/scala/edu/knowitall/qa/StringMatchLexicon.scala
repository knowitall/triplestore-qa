package edu.knowitall.qa

import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.triplestore.Search.FieldPhrase
import edu.knowitall.triplestore.Search.Field._
import edu.knowitall.triplestore.Search.Disjunction
import edu.knowitall.triplestore.StrSim

case class StringMatchLexicon(client: TriplestoreClient) extends Lexicon {
  
  val stops = StrSim.stops
  
  def wordsToStr(words: IndexedSeq[QToken]) = words.mkString(" ")
  
  def isStop(words: IndexedSeq[QToken]): Boolean = {
    val ss = words.map(_.toString.toLowerCase()).toSet
    ss.subsetOf(stops)
  }
  
  def hasEnt(words: IndexedSeq[QToken]): Boolean = {
    if (isStop(words)) return false
    val s = wordsToStr(words)
    val q1 = FieldPhrase(arg1, s)
    val q2 = FieldPhrase(arg2, s)
    val q = Disjunction(q1, q2)
    client.count(q) > 0
  }
  
  def hasRel(words: IndexedSeq[QToken]): Boolean = {
    val s = wordsToStr(words)
    val q = FieldPhrase(rel, s)
    client.count(q) > 0
  }
  
  type QWords = IndexedSeq[QWord]
  type QTokens = IndexedSeq[QToken]
  def get(words: IndexedSeq[QToken]): Iterable[LexItem] = {
    words match {
      case ws: QWords => getRel(ws) ++ getEnt(ws)
      case _ => getQuestion(words)
    }
  }
  
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem] = {
    if (hasRel(words)) {
      val s = wordsToStr(words)
      val item1 = RelItem(words, s, Arg1First)
      val item2 = RelItem(words, s, Arg2First)
      List(item1, item2)
    } else {
      Nil
    }
  }
  
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem] = {
    if (hasEnt(words)) {
      val s = wordsToStr(words)
      val item = EntItem(words, s)
      List(item)
    } else {
      Nil
    }
  }
  
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem] = {
    val item1 = QuestionItem(words, Arg1First)
    val item2 = QuestionItem(words, Arg2First)
    List(item1, item2)
  }
  
  def has(words: IndexedSeq[QToken]): Boolean = {
    words match {
      case ws: QWords => hasRel(ws) || hasEnt(ws)
      case _ => true
    }
  }

}