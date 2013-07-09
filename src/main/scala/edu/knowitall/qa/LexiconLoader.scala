package edu.knowitall.qa

import scala.io.Source
import java.io.File
import edu.knowitall.common.Resource.using
import scala.collection.JavaConversions._

abstract class LexLoader {
  
  def load: Lexicon
}

class ParalexLoader(val dbVocabFile: File, val lexVocabFile: File, val lexEntryFile: File) extends LexLoader {
  
  class VocabEntry(val id: Int, val tokens: IndexedSeq[QWord]) extends Tuple2[Int, IndexedSeq[QWord]](id, tokens) {
    require(tokens.nonEmpty, s"Empty vocab entry #$id.")
  }
  
  private val spaceRegex = "\\s+".r
  
  private def readVocabLine(str: String): VocabEntry = spaceRegex.split(str).toList match {
    case id :: tokens => new VocabEntry(id.toInt, tokens map QWord.qWordWrap toIndexedSeq)
    case _ => throw new RuntimeException(s"Malformed vocab record: $str")
  }
  
  private def loadVocab(file: File): Map[Int, IndexedSeq[QWord]] = using(Source.fromFile(file)) { source =>
    source.getLines map readVocabLine toMap
  }
  
  lazy val dbVocab = loadVocab(dbVocabFile)
  
  lazy val lexVocab = loadVocab(lexVocabFile)

  private def readLexEntry(str: String): LexItem = spaceRegex.split(str).map(_.toInt) match {
    case Array(lexId, 0, dbId) => EntItem(lexVocab(lexId), dbVocab(dbId).mkString(" "))
    case Array(lexId, 3, order, dbId) => QuestionItem(lexVocab(lexId), ArgOrder.fromInt(order))
  }
  
  lazy val load: Lexicon = {
    
    val foo = io.Source.fromFile(dbVocabFile).getLines map readVocabLine
    throw new RuntimeException(s"Not Implemented")
  }
}