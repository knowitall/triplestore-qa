package edu.knowitall.qa

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.impl.HttpSolrServer

import scala.collection.JavaConverters._

class SolrLexicon(val server: SolrServer) extends WeightedLexicon {

  def this(url: String) = this(new HttpSolrServer(url))
  
  private def queryString(words: IndexedSeq[QToken]) = 
    "+tokens_exact:\"%s\"".format(words.map(_.toString).mkString(" "))

  private def buildQuery(words: IndexedSeq[QToken]): SolrQuery = {
    new SolrQuery(queryString(words))
      .addSort("weight", SolrQuery.ORDER.desc)
  }
  
  private def buildCountQuery(words: IndexedSeq[QToken]): SolrQuery = {
    new SolrQuery(queryString(words)).setRows(0)
  }
  
  private def execQuery(sq: SolrQuery): Iterable[LexItem with Weight] = {
    val resp = server.query(sq)
    resp.getResults().asScala.map(LexItemConverter.docToItem)
  }
  
  def get(words: IndexedSeq[QToken]): Iterable[LexItem with Weight] = {
    
    execQuery(buildQuery(words))
  }
  
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem with Weight] = 
    get(words) filter (_.isInstanceOf[RelItem]) map (_.asInstanceOf[RelItem with Weight])
    
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem with Weight] = 
    get(words) filter (_.isInstanceOf[EntItem]) map (_.asInstanceOf[EntItem with Weight])
    
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem with Weight] = 
    get(words) filter (_.isInstanceOf[QuestionItem]) map (_.asInstanceOf[QuestionItem with Weight])
  
  def has(words: IndexedSeq[QToken]): Boolean = {
    server.query(buildCountQuery(words)).getResults().getNumFound() > 0
  }
}