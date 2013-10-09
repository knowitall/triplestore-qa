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
import org.apache.solr.client.solrj.SolrQuery.SortClause

case class ParaphraseTemplateClient(solrUrl: String, hitLimit: Int = 500) {
  val logger = LoggerFactory.getLogger(this.getClass)
  val server = new HttpSolrServer(solrUrl)
  val searchField = "template1_exact"
  def paraphrases(s: String, limit: Int = hitLimit): List[(String, Double)] = {
    val query = new SolrQuery(s"""${searchField}:"${s}"""")
    query.setRows(hitLimit)
    query.addSort(new SortClause("joint_count", SolrQuery.ORDER.desc))
    logger.info(s"Sending query: ${query.toString()}")
    val resp = server.query(query)
    logger.info(s"Found ${resp.getResults().getNumFound()} hits")
    val pairs = resp.getResults().toList.flatMap(TemplatePair.fromDocument)
    pairs.map(p => (p.template2, p.pmi))
  }
}

case class TemplatePair(template1: String, template2: String, pmi: Double, count1: Double, count2: Double, jointCount: Double) {
  def this(t1: String, t2: String, j: Double, c1: Double, c2: Double) = this(t1, t2, TemplatePair.pmi(j, c1, c2), c1, c2, j)
  def this(t1: String, t2: String, j: String, c1: String, c2: String) = this(t1, t2, j.toDouble, c1.toDouble, c2.toDouble)
}

case object TemplatePair {
  
  def pmi(j: Double, m1: Double, m2: Double): Double = Math.log(j) - Math.log(m1) - Math.log(m2) 
    
  def parseDouble(s: String) = try { Some(s.toDouble) } catch { case e:Throwable => None }
  
  def fromString(s: String): Option[TemplatePair] = {
    s.split("\t", 5) match {
      case Array(t1, t2, js, ms1, ms2) => Some(new TemplatePair(t1, t2, js, ms1, ms2))
      case _ => None
    }
  }
  def fromDocument(doc: SolrDocument): Option[TemplatePair] = {
    val t1obj: Any = doc.getFieldValue("template1")
    val t2obj: Any = doc.getFieldValue("template2")
    val sobj: Any = doc.getFieldValue("pmi")
    val count1obj: Any = doc.getFieldValue("marg_count1")
    val count2obj: Any = doc.getFieldValue("marg_count2")
    val jointobj: Any = doc.getFieldValue("joint_count")
    (t1obj, t2obj, sobj, count1obj, count2obj, jointobj) match {
      case (t1: String, t2: String, s: Float, c1: Float, c2: Float, j: Float) => Some(TemplatePair(t1, t2, s, c1, c2, j))
      case _ => None
    }
  }
}

class ParalexIndexer(server: SolrServer) {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def pairToDoc(pair: TemplatePair): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("template1", pair.template1, pair.pmi.toFloat)
    doc.addField("template2", pair.template2, pair.pmi.toFloat)
    doc.addField("score", pair.pmi)
    doc.addField("id", s"${pair.template1}|${pair.template2}")
    doc.addField("marg_count1", s"${pair.count1}")
    doc.addField("marg_count2", s"${pair.count2}")
    doc.addField("joint_count", s"${pair.jointCount}")
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
 * pipe (template1, template2, joint count, marginal count1, marginal count2)
 * triples into stdin.
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