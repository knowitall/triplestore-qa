package edu.knowitall.qa

import scala.io.Source
import java.io.File
import scala.collection.JavaConversions._
import edu.knowitall.common.Resource.using
import java.util.concurrent.atomic.AtomicInteger 

trait Weight {
  def weight: Double
}

class EvalLexiconLoader(
    val dbVocabFile: File, 
    val lexVocabFile: File,
    val lexWeightFile: File,
    val lexEntryFile: File) extends Iterable[LexItem with Weight] {
  
  def this(rootPath: File) = this(
    dbVocabFile = new File(rootPath, "database/vocab.txt"),
    lexVocabFile = new File(rootPath, "lexicons/paralex/vocab.txt"),
    lexWeightFile = new File(rootPath, "weights/paralex.txt"),
    lexEntryFile = new File(rootPath, "lexicons/paralex/lexicon.txt"))
  
  private val entryRegex = "(\\d+)\\s+(.+)".r
  
  private val splitRegex = "\\s+".r
  
  private def loadEntry(str: String): (Int, String) = str match {
    case entryRegex(id, tokens) => (id.toInt, tokens)
    case _ => throw new RuntimeException(s"Malformed vocab record: $str")
  }
  
  private val dbVocab = using(Source.fromFile(dbVocabFile)) { source => 
  	System.err.println(s"Loading DB vocab...")
    source.getLines map loadEntry toMap
  }
  
  private val lexVocab = using(Source.fromFile(lexVocabFile)) { source => 
      System.err.println(s"Loading Lexicon vocab...")
      source.getLines map loadEntry map { case (id, string) => 
        (id, splitRegex.split(string) map QWord.qWordWrap) 
    } toMap
  }
  
  private val lexWeights = using(Source.fromFile(lexWeightFile)) { source => 
      System.err.println(s"Loading Lexicon weights...")
      source.getLines map splitRegex.split map { case Array(weight, lexId, _*) => 
        (lexId.toInt, weight.toDouble) 
    } toMap
  }
  
  private def getWeight(lexId: Int) = lexWeights.getOrElse(lexId, 0.0)

  private def withVars(words: IndexedSeq[QWord]) = words map(_.word) map QToken.qTokenWrap 
  
  private def loadEntItem(lexId: Int, dbId: Int) = {
    new EntItem(lexVocab(lexId), dbVocab(dbId)) 
    with Weight { val weight = getWeight(lexId) }
  }
  
  private def loadRelItem(lexId: Int, dbId: Int, order: Int) = {
    new RelItem(lexVocab(lexId), dbVocab(dbId), ArgOrder.fromInt(order)) 
    with Weight { val weight = getWeight(lexId) }
  }
  
  private def loadQuestionItem(lexId: Int, order: Int) = {
    new QuestionItem(withVars(lexVocab(lexId)), ArgOrder.fromInt(order)) 
    with Weight { val weight = getWeight(lexId) }
  }
  
  private def readLexEntry(str: String): Option[LexItem with Weight] = splitRegex.split(str).map(_.toInt) match {

    case Array(lexId, 0, dbId) => Some(loadEntItem(lexId, dbId))

    case Array(lexId, 3, order, dbId) => withVars(lexVocab(lexId)) match {
      case tokens if tokens.forall(_.isInstanceOf[QWord]) => Some(loadRelItem(lexId, dbId, order))
      
      case tokens => 
        None // Some(QuestionItem(tokens, ArgOrder.fromInt(order)))
    }

    case Array(lexId, 1, order) => Some(loadQuestionItem(lexId, order))
    
    case _ => throw new RuntimeException(s"Unrecognized lexicon encoding: $str")
  }
  
  override def iterator = new Iterator[LexItem with Weight]() {
    System.err.println("Loading Lexicon...")
    private val lexSource = io.Source.fromFile(lexEntryFile)
    private val lexItemIterator = lexSource.getLines flatMap readLexEntry
    def hasNext = if (!lexItemIterator.hasNext) { lexSource.close(); false } else true
    def next = { hasNext; lexItemIterator.next() }
  }
}