package edu.knowitall.eval.qa

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import java.io.InputStream
import scala.io.Source
import java.io.FileInputStream
import java.io.PrintWriter
import java.io.File

trait QAOracle {

  def getLabel(q: String, a: String): Option[Boolean]
  
  def isCorrect(question: String, answer: String): Boolean = {
    val (q, a) = QAOracle.normalizePair(question, answer)
    getLabel(q, a) match {
    	case Some(label) => label
    	case _ => throw new IllegalStateException(s"No label for ($question, $answer)")
    }
  }
  
  def hasLabel(question: String, answer: String): Boolean = {
    val (q, a) = QAOracle.normalizePair(question, answer)
    getLabel(q, a) match {
      case Some(label) => true
      case _ => false
    }
  }

}

trait UpdateableQAOracle extends QAOracle {
  def save
  def update(q: String, a: String, label: Boolean)
}

object QAOracle {
    
  val tokenizer = new ClearTokenizer()
  val tagger = new StanfordPostagger()
  val lemmatizer = new MorphaStemmer()
  
  def normalizePair(q: String, a: String) = (normalize(q), normalize(a))
  
  def normalize(s: String): String = {
    val tokens = tokenizer(s)
    val tagged = tagger.postagTokens(tokens)
    val result = tagged.map(lemmatizer.lemmatizePostaggedToken(_).lemma.toLowerCase)
    result.mkString(" ")
  }
  
  def readLine(line: String): (String, String, Boolean) = {
    val fields = line.split("\t", 4)
    fields match {
      case Array(tag, l, q, a) => (normalize(q), normalize(a), getBoolean(l))
      case _ => throw new IllegalArgumentException(s"Could not parse line: '$line'")
    }
  }
  def getBoolean(s: String) = s.toLowerCase() match {
    case "0" => false
    case "false" => false
    case _ => true
  }
  
  def labelsFromInputStream(is: InputStream) = {
    val triples = Source.fromInputStream(is, "UTF8").getLines.filter(_.startsWith("LABEL\t")).map(readLine)
    triples.map(triple => (triple._1, triple._2) -> triple._3).toMap
  }
  
  def fromFile(fn: String) = labelsFromInputStream(new FileInputStream(fn))
} 

class FileQAOracle(path: String) extends UpdateableQAOracle {
  val file = new File(path)
  val labels = if (file.exists()) {
    scala.collection.mutable.Map(QAOracle.fromFile(path).toSeq: _*)
  } else {
    scala.collection.mutable.Map[(String, String), Boolean]()
  }
  override def getLabel(q: String, a: String) = labels.get((q, a))
  def save = {
    val output = new PrintWriter(path)
    for (((q, a), l) <- labels) output.println(s"LABEL\t$l\t$q\t$a")
    output.close()
  }
  def update(q: String, a: String, label: Boolean) = labels += ((q, a) -> label)
}