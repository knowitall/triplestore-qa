package edu.knowitall.execution.generalize

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import java.util.ArrayList
import scala.collection.JavaConverters._
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.stem.MorphaStemmer

import edu.knowitall.tool.postag.PostaggedToken

class ParaphraseFinder(solrServer: SolrServer) {

  def getQuery(question: String) = new SolrQuery("question:\"%s\"".format(question))
  
  def paraphrases(question: String): Set[String] = {

    val result = solrServer.query(getQuery(question))
    val docList = result.getResults()
    // SolrDocumentList is a loose implementation of a java collection interface... 
    val docs = for (
      i <- 0 until docList.size();
      doc = docList.get(i)
    ) yield doc

    docs.flatMap(paraphrasesFromDoc).toSet
  }
  
  def paraphrasesFromDoc(doc: SolrDocument): Seq[String] = {
    val qs = doc.getFieldValue("question").asInstanceOf[ArrayList[String]]
    qs.asScala.toSeq
  }
  
  val postagger = new ClearPostagger()
  val stemmer = new MorphaStemmer()
  
  def postagAndStem(s: String) = postagger(s) map stemmer.stemPostaggedToken
  
  def paraphrasesTermVector(question: String): Map[String, Int] = {
    val tokens = paraphrases(question).toSeq.flatMap(postagAndStem)
    tokens.groupBy(_.lemma).map(p => (p._1, p._2.size))
  }
}

object ParaphraseFinder {
  
  import org.apache.solr.client.solrj.impl.HttpSolrServer
  
  def main(args: Array[String]): Unit = {
    
    val solrServer = new HttpSolrServer("http://rv-n12.cs.washington.edu:8982/solr/wikianswers")
    val pfinder = new ParaphraseFinder(solrServer)
    
    val q = "What was the release date for halo reach?"
    
    val qts = pfinder.postagAndStem(q)
      
    val ps = pfinder.paraphrases(q)
    
    ps.zipWithIndex foreach { case (p, index) => println(s"$index\t$p") }
      
    val pvec = pfinder.paraphrasesTermVector(q)
    
    println()
    
    pvec.toSeq.sortBy(-_._2).foreach { case (tok, freq) =>
      println(s"$freq -> $tok")  
    }
    
    println()
    
    qts.foreach { qt =>
      val freq = pvec(qt.lemma)
      println(s"$freq -> ${qt.lemma}")
    }
  }
}