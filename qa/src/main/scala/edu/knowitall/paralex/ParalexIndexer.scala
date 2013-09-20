package edu.knowitall.paralex
import scala.io.Source
import org.apache.solr.common.SolrDocument
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.client.solrj.SolrServer
import org.slf4j.LoggerFactory
import org.apache.solr.client.solrj.impl.HttpSolrServer

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
  }
} 