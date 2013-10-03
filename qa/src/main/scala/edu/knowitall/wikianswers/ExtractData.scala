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
      case Some(doc) => for ((q1, q2) <- WikiAnswersDoc.getQuestionParaphrases(doc)) yield (s"$q1", s"$q2")
      case None => List.empty
    }
  }
  
  def allPairs(items: List[String]): List[(String, String)] = 
    for (x <- items; y <- items) yield (x, y)
    
  def distinctSorted(x: (String, Iterable[String])) = 
    x._2.toList.take(500).distinct.sorted
  
  def run() {
      
    lazy val tagger = new StanfordPostagger()
    lazy val tokenizer = new ClearTokenizer()
    def processQuestion(q: String): Option[String] = try {
      val tokens = tokenizer(q)
      val tagged = tagger.postagTokens(tokens)
      val lemmatized = tagged.map(MorphaStemmer.lemmatizePostaggedToken)
      val strTokens = tokens.map(_.string).mkString(" ")
      val strTags = tagged.map(_.postag).mkString(" ")
      val strLems = lemmatized.map(_.lemma.toLowerCase).mkString(" ")
      val result = s"${q}\t${strTokens}\t${strLems}\t${strTags}"
      Some(result)
    } catch {
      case e: Throwable => { e.printStackTrace(System.err); None }
    }
    def processQuestionCluster(qs: List[String]): String = {
      val proccessed = qs.flatMap(processQuestion)
      proccessed.mkString("\t")
    }
    
    val lines = textFromLzo(args(0))
    val paras = lines.mapFlatten(getParaphrases)
    val plainClusters = paras.groupByKey.map(distinctSorted).distinct
    val procClusters = plainClusters.map(processQuestionCluster)
    persist(procClusters.toTextFile(args(1), true))
  }

}

object ScoobiUtils {

    
  def textFromLzo(path: String) = 
    TextInput.fromTextSource(
      new TextSource(Seq(path),
      inputFormat = classOf[LzoTextInputFormat].asInstanceOf[Class[org.apache.hadoop.mapreduce.lib.input.TextInputFormat]]))
}
