package edu.knowitall.qa

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.impl.HttpSolrServer

import scala.collection.JavaConverters._

class SolrLexicon(val server: SolrServer) extends Lexicon {

  def this(url: String) = this(new HttpSolrServer(url))
  
  private def queryString(words: IndexedSeq[QToken]) = s"tokens:${words.map(_.toString).mkString(" ")}"

  private def buildQuery(words: IndexedSeq[QToken]): SolrQuery = {
    new SolrQuery(queryString(words))
      .setSort("weight", SolrQuery.ORDER.desc)
      .setFields(LexItemConverter.allFields.toSeq: _*)
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
  
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem] = 
    get(words) filter (_.isInstanceOf[RelItem]) map (_.asInstanceOf[RelItem])
    
  def getEnt(words: IndexedSeq[QWord]) = 
    get(words) filter (_.isInstanceOf[EntItem]) map (_.asInstanceOf[EntItem])
    
  def getQuestion(words: IndexedSeq[QToken]) = 
    get(words) filter (_.isInstanceOf[QuestionItem]) map (_.asInstanceOf[QuestionItem])
  
  def has(words: IndexedSeq[QToken]): Boolean = {
    server.query(buildCountQuery(words)).getResults().getNumFound() > 0
  }
}