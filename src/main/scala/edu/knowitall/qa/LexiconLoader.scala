package edu.knowitall.qa

import java.io.File

abstract class LexLoader {
  
  def load: Lexicon
}

class ParalexLoader(val dbVocabFile: File, val lexVocabFile: File, val lexEntryFile: File) {
  
  class VocabEntry(val id: Int, val lexItem: LexItem) extends Tuple2[Int, LexItem](id, lexItem)
  
  private val spaceRegex = "\\s+".r
  
  private def readVocabLine(str: String): VocabEntry = spaceRegex.split(str).toList match {
    case id :: tokens => {
      throw new RuntimeException(s"Not Implemented")
    }
    case _ => throw new RuntimeException(s"Malformed vocab record: $str")
  }
  
  
  
  lazy val load: Lexicon = {
    
    val foo = io.Source.fromFile(dbVocabFile).getLines map readVocabLine
    val map = foo.toMap
    throw new RuntimeException(s"Not Implemented")
  }
}