package edu.knowitall.qa

import scala.io.Source
import java.io.File
import scala.collection.JavaConversions._
import edu.knowitall.common.Resource.using
import java.util.concurrent.atomic.AtomicInteger 

class EvalLexiconLoader(val dbVocabFile: File, val lexVocabFile: File, val lexEntryFile: File) extends Iterable[LexItem] {
  
  private val entryRegex = "(\\d+)\\s+(.+)".r
  
  private def loadEntry(str: String): (Int, String) = str match {
    case entryRegex(id, tokens) => (id.toInt, tokens)
    case _ => throw new RuntimeException(s"Malformed vocab record: $str")
  }
  
  val dbVocab = using(Source.fromFile(dbVocabFile)) { source => 
  	System.err.println(s"Loading DB vocab...")
    source.getLines map loadEntry toMap
  }
  
  private val splitRegex = "\\s+".r
  
  val lexVocab = using(Source.fromFile(lexVocabFile)) { source => 
      System.err.println(s"Loading Lexicon vocab...")
      source.getLines map loadEntry map { case (id, string) => 
        (id, splitRegex.split(string) map QWord.qWordWrap) 
    } toMap
  } 

  private def withVars(words: IndexedSeq[QWord]) = words map(_.word) map QToken.qTokenWrap 
  
  private def readLexEntry(str: String): Option[LexItem] = splitRegex.split(str).map(_.toInt) match {

    case Array(lexId, 0, dbId) => Some(EntItem(lexVocab(lexId), dbVocab(dbId)))

    case Array(lexId, 3, order, dbId) => withVars(lexVocab(lexId)) match {
      case tokens if tokens.forall(_.isInstanceOf[QWord]) => 
        Some(RelItem(lexVocab(lexId), dbVocab(dbId), ArgOrder.fromInt(order)))
      case tokens => 
        None // Some(QuestionItem(tokens, ArgOrder.fromInt(order)))
    }

    case Array(lexId, 1, order) => Some(QuestionItem(withVars(lexVocab(lexId)), ArgOrder.fromInt(order)))
    
    case _ => throw new RuntimeException(s"Unrecognized lexicon encoding: $str")
  }

  class LexIterator(lexSource: Source) extends Iterator[LexItem] {
    System.err.println("Loading Lexicon...")
    val lexItemIterator = lexSource.getLines flatMap readLexEntry
    def hasNext = if (!lexItemIterator.hasNext) { lexSource.close(); false } else true

    def next = if (hasNext) lexItemIterator.next()
      else throw new NoSuchElementException("Empty LexItem iterator")
  }
  
  override def iterator = new LexIterator(Source.fromFile(lexEntryFile))
}