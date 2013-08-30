package edu.knowitall.execution.synonyms

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import edu.knowitall.parsing.QWord
import edu.knowitall.parsing.{LexItem, EntItem, RelItem}
import edu.knowitall.parsing.SolrLexicon
import scala.collection.JavaConverters._
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import edu.knowitall.parsing.EntItem
import edu.knowitall.parsing.LexItem
import edu.knowitall.parsing.RelItem

class LexiconSynonyms(val server: SolrServer) {

  def this(url: String = "http://rv-n13.cs.washington.edu:8888/solr") = this(new HttpSolrServer(url))
  
  val solrLexicon = new SolrLexicon(server)

  private def buildQuery(field: String, value: String): SolrQuery = {
    val queryString = "+%s:\"%s\"".format(field, value)
    new SolrQuery(queryString)
      .addSort("weight", SolrQuery.ORDER.desc).setRows(100)
  }
  
  def lookupLexItems(str: String): Seq[LexItem] = {
    val words = str.split(" ").map(QWord.qWordWrap)
    val lexItems = solrLexicon.get(words)
    lexItems.toSeq
  }
  
  private def getTokensFromQuery(sq: SolrQuery): Iterable[String] = {
    val resp = server.query(sq)
    resp.getResults().asScala.map(d => d.getFieldValue("tokens_exact").asInstanceOf[String])
  }
  
  def tokensForLexItem(lexItem: LexItem): Iterable[String] = {
    lexItem match {
      case e: EntItem => getTokensFromQuery(buildQuery("entity", e.entity))
      case r: RelItem => getTokensFromQuery(buildQuery("relation", r.relation))
      case _ => Nil
    }
  }
  
  def synonymsFor(str: String): Seq[String] = {
    val lexItems = lookupLexItems(str) flatMap { 
      case e: EntItem => Some(e.entity)
      case r: RelItem => Some(r.relation)
      case _ => Nil
    } 
    val strings = lexItems map { s => s.replaceAll("-", " ").dropRight(2) } 
    strings.distinct.take(5)
  }
  
  def relSynonymsFor(str: String): Seq[String] = {
    val lexItems = lookupLexItems(str) flatMap { 
      case e: EntItem => None
      case r: RelItem => Some(r.relation)
      case _ => Nil
    } 
    val strings = lexItems map { s => s.replaceAll("-", " ").dropRight(2) } 
    strings.distinct.take(5)
  }
  
  def entSynonymsFor(str: String): Seq[String] = {
    val lexItems = lookupLexItems(str) flatMap { 
      case e: EntItem => Some(e.entity)
      case r: RelItem => None
      case _ => Nil
    } 
    val strings = lexItems map { s => s.replaceAll("-", " ").dropRight(2) } 
    strings.distinct.take(5)
  }
}

object LexiconSynonymsText extends App {
  
  val synServer = new LexiconSynonyms()
  
  val relTestSet = Seq("invent")
  val entTestSet = Nil
  
  val relSyns = relTestSet.map(w => (w, synServer.relSynonymsFor(w).take(10)))
  val entSyns = entTestSet.map(w => (w, synServer.entSynonymsFor(w).take(10)))
  
  (relSyns ++ entSyns).foreach { case (w, syns) => 
    println(w)
    syns.foreach { s => println("   " + s)}
  }
}