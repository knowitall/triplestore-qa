package edu.knowitall.eval

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import java.io.InputStream
import scala.io.Source
import java.io.FileInputStream
import java.io.PrintWriter
import java.io.File

trait Oracle {

  def getLabel(input: String, output: String): Option[Boolean]
  
  def isCorrect(input: String, output: String): Boolean = {
    val (i, o) = Oracle.normalizePair(input, output)
    getLabel(i, o) match {
    	case Some(label) => label
    	case _ => throw new IllegalStateException(s"No label for ($input, $output)")
    }
  }
  
  def hasLabel(input: String, output: String): Boolean = {
    val (i, o) = Oracle.normalizePair(input, output)
    getLabel(i, o) match {
      case Some(label) => true
      case _ => false
    }
  }

}

trait UpdateableOracle extends Oracle {
  def save
  def update(input: String, output: String, label: Boolean)
}

object Oracle {
    
  val tokenizer = new ClearTokenizer()
  val tagger = new StanfordPostagger()
  val lemmatizer = new MorphaStemmer()
  
  def normalizePair(input: String, output: String) = (normalize(input), normalize(output))
  
  def normalize(s: String): String = {
    val tokens = tokenizer(s)
    if (tokens.size > 0) {
      val tagged = tagger.postagTokens(tokens)
      val result = tagged.map(lemmatizer.lemmatizePostaggedToken(_).lemma.toLowerCase)
      result.mkString(" ")
    } else {
      s.toLowerCase().mkString(" ")
    }
  }
  
  def readLine(line: String): (String, String, Boolean) = {
    val fields = line.split("\t", 4)
    fields match {
      case Array(tag, l, i, o) => (normalize(i), normalize(o), getBoolean(l))
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

class FileOracle(path: String) extends UpdateableOracle {
  val file = new File(path)
  val labels = if (file.exists()) {
    scala.collection.mutable.Map(Oracle.fromFile(path).toSeq: _*)
  } else {
    scala.collection.mutable.Map[(String, String), Boolean]()
  }
  override def getLabel(input: String, output: String) = labels.get((input, output))
  def save = {
    val output = new PrintWriter(path)
    for (((i, o), l) <- labels) output.println(s"LABEL\t$l\t$i\t$o")
    output.close()
  }
  def update(i: String, o: String, label: Boolean) = labels += ((i, o) -> label)
}