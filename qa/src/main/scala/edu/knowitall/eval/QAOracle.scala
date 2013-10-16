package edu.knowitall.eval

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import java.io.InputStream
import scala.io.Source
import java.io.FileInputStream
import java.io.PrintWriter

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
    val fields = line.split("\t", 3)
    fields match {
      case Array(q, a, l) => (normalize(q), normalize(a), l.toBoolean)
      case _ => throw new IllegalArgumentException(s"Could not parse line: '$line'")
    }
  }
  
  def labelsFromInputStream(is: InputStream) = {
    val triples = Source.fromInputStream(is).getLines.map(readLine)
    triples.map(triple => (triple._1, triple._2) -> triple._3).toMap
  }
  
  def fromFile(fn: String) = labelsFromInputStream(new FileInputStream(fn))
} 

class FileQAOracle(path: String) extends QAOracle {
  val labels =  scala.collection.mutable.Map(QAOracle.fromFile(path).toSeq: _*)
  override def getLabel(q: String, a: String) = labels.get((q, a))
  def save = {
    val output = new PrintWriter(path)
    for (((q, a), l) <- labels) output.println(s"$q\t$a\t$l")
    output.close()
  }
  def update(q: String, a: String, label: Boolean) = labels += ((q, a) -> label)
}