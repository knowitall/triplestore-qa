package edu.knowitall.wikianswers

import com.nicta.scoobi.application.ScoobiApp
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.io.text.TextInput

object ProcessClusters extends ScoobiApp {
  
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
    def processQuestionCluster(line: String): String = {
      val qs = line.split("\t")
      val proccessed = qs.flatMap(processQuestion)
      proccessed.mkString("\t")
    }
    val lines = fromTextFile(args(0))
    val procClusters = lines.map(processQuestionCluster)
    persist(procClusters.toTextFile(args(1), true))
  }
  

}