package edu.knowitall.parsing

import scala.io.Source
import java.io.File
import scala.collection.JavaConversions._
import edu.knowitall.common.Resource.using
import java.util.concurrent.atomic.AtomicInteger 

class EvalLexiconLoader(
    val dbVocabFile: File, 
    val lexVocabFile: File,
    val lexWeightFile: File,
    val lexEntryFile: File) extends Iterable[LexItem] {
  
  def this(rootPath: File) = this(
    dbVocabFile = new File(rootPath, "database/vocab.txt"),
    lexVocabFile = new File(rootPath, "lexicons/paralex/vocab.txt"),
    lexWeightFile = new File(rootPath, "weights/paralex.txt"),
    lexEntryFile = new File(rootPath, "lexicons/paralex/lexicon.txt"))
  
  def this(path: String) = this(new File(path))
    
  private val entryRegex = "(\\d+)\\s+(.+)".r
  
  private val splitRegex = "\\s+".r
  
  private def loadEntry(str: String): (Int, String) = str match {
    case entryRegex(id, tokens) => (id.toInt, tokens)
    case _ => throw new RuntimeException(s"Malformed vocab record: $str")
  }
  
  private val dbVocab = using(Source.fromFile(dbVocabFile, "UTF8")) { source => 
  	System.err.println(s"Loading DB vocab...")
    source.getLines.map(loadEntry).toMap
  }
  
  private val lexVocab = using(Source.fromFile(lexVocabFile, "UTF8")) { source => 
      System.err.println(s"Loading Lexicon vocab...")
      source.getLines.map(loadEntry).map({ case (id, string) => 
        (id, splitRegex.split(string) map QWord.qWordWrap) 
    }).toMap
  }
  
  private val lexWeights = using(Source.fromFile(lexWeightFile, "UTF8")) { source => 
      System.err.println(s"Loading Lexicon weights...")
      source.getLines.map(l => splitRegex.split(l).toList).map({ case weight :: lexItemEncoding => 
        require(lexItemEncoding.length >= 3, "Unrecognized lexitem encoding.")
        (lexItemEncoding.map(_.toInt), weight.toDouble) 
    }).toMap
  }
  
  private def getWeight(lexItemEncoding: List[Int]) = lexWeights.getOrElse(lexItemEncoding, 0.0)

  private def withVars(words: IndexedSeq[QWord]) = words map(_.word) map QToken.qTokenWrap 
  
  private def loadEntItem(lexId: Int, dbId: Int) = {
    val weightKey = lexId :: 0 :: dbId :: Nil
    new EntItem(lexVocab(lexId), dbVocab(dbId), getWeight(weightKey))
  }
  
  private def loadRelItem(lexId: Int, dbId: Int, order: Int) = {
    val weightKey = lexId :: 3 :: order :: dbId :: Nil
    new RelItem(lexVocab(lexId), dbVocab(dbId), ArgOrder.fromInt(order), getWeight(weightKey))
  }
  
  private def loadQuestionItem(lexId: Int, order: Int) = {
    val weightKey = lexId :: 1 :: order :: Nil
    new QuestionItem(withVars(lexVocab(lexId)), ArgOrder.fromInt(order), getWeight(weightKey))
  }
  
  private def loadQuestionRelItem(lexId: Int, dbId: Int, order: Int) = {
    val weightKey = lexId :: 3 :: order :: dbId :: Nil
    new QuestionRelItem(withVars(lexVocab(lexId)), dbVocab(dbId), ArgOrder.fromInt(order), getWeight(weightKey))
  }
  
  private def readLexEntry(str: String): Option[LexItem] = splitRegex.split(str).map(_.toInt) match {

    // Entity Item encoding
    case Array(lexId, 0, dbId) => Some(loadEntItem(lexId, dbId))

    // RelItem or QuestionRelItem encoding
    case Array(lexId, 3, order, dbId) => withVars(lexVocab(lexId)) match {
      
      case tokens if tokens.forall(_.isInstanceOf[QWord]) => Some(loadRelItem(lexId, dbId, order))
      case tokens => Some(loadQuestionRelItem(lexId, dbId, order))
    }

    // QuestionItem encoding
    case Array(lexId, 1, order) => Some(loadQuestionItem(lexId, order))
    
    case _ => throw new RuntimeException(s"Unrecognized lexicon encoding: $str")
  }
  
  override def iterator = new Iterator[LexItem]() {
    System.err.println("Loading Lexicon...")
    private val lexSource = io.Source.fromFile(lexEntryFile)
    private val lexItemIterator = lexSource.getLines flatMap readLexEntry
    
    override def hasNext = if (!lexItemIterator.hasNext) { lexSource.close(); false } else true
    override def next = { hasNext; lexItemIterator.next() }
  }
}