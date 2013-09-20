package edu.knowitall.paralex
import scala.io.Source
import scala.collection.JavaConversions._
import org.apache.solr.common.SolrDocument
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.client.solrj.SolrServer
import org.slf4j.LoggerFactory
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery

case class ParaphraseTemplateClient(solrUrl: String, hitLimit: Int) {
  val server = new HttpSolrServer(solrUrl)
  val searchField = "template1_exact"
  def paraphrases(s: String, limit: Int): List[(String, Double)] = {
    val query = new SolrQuery(s"${searchField}:(${s})")
    query.setRows(hitLimit)
    val resp = server.query(query)
    val pairs = resp.getResults().toList.flatMap(TemplatePair.fromDocument)
    pairs.map(p => (p.template2, p.score))
  }
}

case class TemplatePair(template1: String, template2: String, score: Double)

case object TemplatePair {
  def parseDouble(s: String) = try { Some(s.toDouble) } catch { case e:Throwable => None }
  def fromString(s: String): Option[TemplatePair] = {
    val fields = s.split("\t")
    fields match {
      case Array(t1, t2, scoreStr) => parseDouble(scoreStr) match {
        case Some(score) => Some(TemplatePair(t1, t2, score))
        case _ => None
      }
      case _ => None
    }
  }
  def fromDocument(doc: SolrDocument): Option[TemplatePair] = {
    val t1obj: Any = doc.getFieldValue("template1")
    val t2obj: Any = doc.getFieldValue("template2")
    val sobj: Any = doc.getFieldValue("score")
    (t1obj, t2obj, sobj) match {
      case (t1: String, t2: String, s: Float) => Some(TemplatePair(t1, t2, s))
      case _ => None
    }
  }
}

class ParalexIndexer(server: SolrServer) {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def pairToDoc(pair: TemplatePair): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("template1", pair.template1)
    doc.addField("template2", pair.template2)
    doc.addField("score", pair.score)
    doc.addField("id", s"${pair.template1}|${pair.template2}")
    doc
  }
  
  def indexPair(pair: TemplatePair) = {
    server.add(pairToDoc(pair))
  }
  
  def indexLine(line: String) = TemplatePair.fromString(line) match {
    case Some(pair) => indexPair(pair)
    case None => logger.warn(s"Unable to parse line: $line")
  } 

}

/**
 * pipe tab-separated (template1, template2, score) triples into stdin
 * pass url to solr as argument
 */
object ParalexIndexer extends App {
  val logger = LoggerFactory.getLogger(this.getClass)
  override def main(args: Array[String]) {
     val groupSize = 100000
     val solrUrl = args(0)
     val server = new ConcurrentUpdateSolrServer(solrUrl, 1000, 4)
     val indexer = new ParalexIndexer(server)
     val lines = Source.fromInputStream(System.in, "UTF8").getLines
     val groups = lines.grouped(groupSize)
     for (group <- groups; line <- group.par) indexer.indexLine(line)
     server.commit()
  }
} 