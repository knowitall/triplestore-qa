package edu.knowitall.wikianswers

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.Scoobi.ScoobiApp
import com.nicta.scoobi.io.text.TextInput
import com.nicta.scoobi.io.text.TextSource
import com.hadoop.mapreduce.LzoTextInputFormat
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.core.DList
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.postag.PostaggedToken

object ExtractData extends ScoobiApp {

  import ScoobiUtils._
  
  def getParaphrases(line: String): List[(String, String)] = {
    WikiAnswersDoc.lineToDoc(line) match {
      case Some(doc) => for ((q1, q2) <- WikiAnswersDoc.getQuestionParaphrases(doc)) yield (s"q:$q1", s"q:$q2")
      case None => List.empty
    }
  }
  
  def getAnswers(line: String): List[(String, String)] = {
    WikiAnswersDoc.lineToDoc(line) match {
      case Some(doc) => {
        val qas = WikiAnswersDoc.getQuestionAnswers(doc)
        qas.map(qa => (s"q:${qa.question}", s"a:${qa.answer}"))
      }
      case _ => List()
    }
  }
  
  def allPairs(items: List[String]): List[(String, String)] = 
    for (x <- items; y <- items) yield (x, y)
    
  def distinctSorted(x: (String, Iterable[String])) = 
    x._2.toList.take(500).distinct.sorted
  
  def run() {
    val lines = textFromLzo(args(0))
    val paras = lines.mapFlatten(getParaphrases)
    val qas = lines.mapFlatten(getAnswers)
    val plainClusters = (paras ++ qas).groupByKey.map(distinctSorted).distinct.map(_.mkString("\t"))
    persist(plainClusters.toTextFile(args(1), true))
  }

}

object ScoobiUtils {

    
  def textFromLzo(path: String) = 
    TextInput.fromTextSource(
      new TextSource(Seq(path),
      inputFormat = classOf[LzoTextInputFormat].asInstanceOf[Class[org.apache.hadoop.mapreduce.lib.input.TextInputFormat]]))
}
