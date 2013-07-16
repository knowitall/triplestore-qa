package edu.knowitall.parsing

import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.Search.FieldKeywords
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.{arg1, rel, arg2}
import edu.knowitall.execution.Search.Disjunction
import edu.knowitall.execution.StrSim
import edu.knowitall.triplestore.SolrClient

case class StringMatchLexicon(client: TriplestoreClient, minCount: Integer = 100) extends Lexicon {
  
  val stops = StrSim.stops ++ Set("who", "what", "where", "when", "why", "how",
      "the", "can", "could", "will", "wo", "n't", "not", "of", "a", "an")
  
  def wordsToStr(words: IndexedSeq[QToken]) = words.mkString(" ")
  
  def isStop(words: IndexedSeq[QToken]): Boolean = {
    val ss = words.map(_.toString.toLowerCase()).toSet
    ss.subsetOf(stops)
  }
  
  def hasEnt(words: IndexedSeq[QToken]): Boolean = {
    if (isStop(words)) return false
    val s = wordsToStr(words)
    val q1 = FieldKeywords(arg1, s)
    val q2 = FieldKeywords(arg2, s)
    val q = Disjunction(q1, q2)
    client.count(q) > minCount
  }
  
  def hasRel(words: IndexedSeq[QToken]): Boolean = {
    if (isStop(words)) return false
    val s = wordsToStr(words)
    val q = FieldKeywords(rel, s)
    client.count(q) > minCount
  }
  
  def get(words: IndexedSeq[QToken]): Iterable[LexItem] = { 
    if (allQWords(words)) {
      val qwords = toQWords(words)
      getRel(qwords) ++ getEnt(qwords)
    } else {
      getQuestion(words)
    }
  }
  
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem] = {
    if (hasRel(words)) {
      val s = wordsToStr(words)
      val item1 = RelItem(words, s, Arg1First, weight=0)
      val item2 = RelItem(words, s, Arg2First, weight=0)
      List(item1, item2)
    } else {
      Nil
    }
  }
  
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem] = {
    if (hasEnt(words)) {
      val s = wordsToStr(words)
      val item = EntItem(words, s, 0)
      List(item)
    } else {
      Nil
    }
  }
  
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem] = {
    List(QuestionItem(words, Arg1First, weight=0))
  }
  
  def getQuestionRel(words: IndexedSeq[QToken]): Iterable[QuestionRelItem] = Nil
  
  def has(words: IndexedSeq[QToken]): Boolean = {
    if (allQWords(words)) {
      val qwords = toQWords(words)
      hasRel(qwords) || hasEnt(qwords)
    } else {
      true
    }
  }

}

object StringMatchLexicon extends App {
  val client = SolrClient("http://rv-n12:8983/solr/triplestore", 100)
  val lexicon = StringMatchLexicon(client)
  val parser = BottomUpParser(lexicon)
  def words(s: String): IndexedSeq[QWord] = s.split(" ").toIndexedSeq.map(QWord(_))
  def parse(s: String) = parser.parse(words(s))
  val s = "when was obama born"
  parse(s).map(_.toString + "\n").map(println)
}