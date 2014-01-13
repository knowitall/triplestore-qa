package edu.knowitall.paraphrasing.template
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
import scala.Option.option2Iterable
import com.typesafe.config.ConfigFactory
import edu.knowitall.util.MathUtils
import edu.knowitall.search.qa.QaAction
import edu.knowitall.triplestore.SolrClient

case class ParaphraseTemplateClient(solrUrl: String, maxHits: Int, scale: Boolean = ParaphraseTemplateClient.scale, timeout: Int = ParaphraseTemplateClient.defaultTimeout) {
  
  def this() = this(ParaphraseTemplateClient.defaultUrl, ParaphraseTemplateClient.defaultMaxHits, ParaphraseTemplateClient.scale)
  
  val logger = LoggerFactory.getLogger(this.getClass)
  val server = new HttpSolrServer(solrUrl)
  server.setConnectionTimeout(timeout)
  server.setSoTimeout(timeout)
  server.setMaxRetries(1)
  val searchField = "template1_exact"
  
  def paraphrases(s: String, argTypes: List[String] = List("anything"), limit: Int = maxHits): List[TemplatePair] = argTypes.flatMap(paraphraseOne(s, _, limit))
   
  def paraphraseOne(s: String, argType: String = "anything", limit: Int = maxHits) = {
    val typePred = s"""typ_exact:"$argType""""
    val qStr = s"""${searchField}:"${s}" AND $typePred"""
    val query = new SolrQuery(SolrClient.fixQuery(qStr))
    query.setRows(maxHits)
    query.addSort(new SortClause("typPmi", SolrQuery.ORDER.desc))
    query.setParam("shards.tolerant", true)
    logger.debug(s"Sending query: ${query.toString()}")
    val resp = server.query(query)
    logger.debug(s"Found ${resp.getResults().getNumFound()} hits")
    val pairs = resp.getResults().toList.flatMap(TemplatePair.fromDocument)
    pairs.map(pair => pair.copy(pmi = scalePmi(pair.pmi)))
  }
    
  private def scalePmi(x: Double): Double = 
    if (scale) MathUtils.clipScale(x, ParaphraseTemplateClient.minPmi, ParaphraseTemplateClient.maxPmi)
    else x
}

case object ParaphraseTemplateClient {
  val conf = ConfigFactory.load()
  val minPmi = conf.getDouble("paraphrase.template.minPmi")
  val maxPmi = conf.getDouble("paraphrase.template.maxPmi")
  val scale = conf.getBoolean("paraphrase.template.scale")
  val defaultUrl = conf.getString("paraphrase.template.url")
  val defaultMaxHits = conf.getInt("paraphrase.template.maxHits")
  val defaultTimeout = conf.getInt("paraphrase.template.timeout")
}

case class TemplatePair(template1: String, template2: String, typ: String, count1: Double, count2: Double, count12: Double, typCount12: Double, typPmi: Double, pmi: Double) extends QaAction

case object TemplatePair {
    
  def fromString(s: String): Option[TemplatePair] = {
    s.split("\t", 9) match {
      case Array(t1, t2, typ, count1, count2, typCount12, count12, typPmi, pmi) =>
         Some(TemplatePair(t1, t2, typ, count1.toDouble, count2.toDouble, typCount12.toDouble, count12.toDouble, typPmi.toDouble, pmi.toDouble))
      case _ => None
    }
  }
  def fromDocument(doc: SolrDocument): Option[TemplatePair] = {
    val t1obj: Any = doc.getFieldValue("template1")
    val t2obj: Any = doc.getFieldValue("template2")
    val typobj: Any = doc.getFieldValue("typ")
    val count1obj: Any = doc.getFieldValue("count1")
    val count2obj: Any = doc.getFieldValue("count2")
    val typCount12Obj: Any = doc.getFieldValue("typCount12")
    val count12Obj: Any = doc.getFieldValue("count12")
    val typPmiObj: Any = doc.getFieldValue("typPmi")
    val pmiObj: Any = doc.getFieldValue("pmi")
    (t1obj, t2obj, typobj, count1obj, count2obj, typCount12Obj, count12Obj, typPmiObj, pmiObj) match {
      case (t1: String, t2: String, typ: String, count1: Float, count2: Float, typCount12: Float, count12: Float, typPmi: Float, pmi: Float) => Some(TemplatePair(t1, t2, typ, count1, count2, typCount12, count12, typPmi, pmi))
      case _ => None
    }
  }
}

class TemplateIndexer(server: SolrServer) {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def pairToDoc(pair: TemplatePair): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("template1", pair.template1)
    doc.addField("template2", pair.template2)
    doc.addField("typ", pair.typ)
    doc.addField("count1", pair.count1)
    doc.addField("count2", pair.count2)
    doc.addField("typCount12", pair.typCount12)
    doc.addField("count12", pair.count12)
    doc.addField("typPmi", pair.typPmi)
    doc.addField("pmi", pair.pmi)
    doc.addField("id", (pair.template1 + pair.template2 + pair.typ).hashCode.toString)
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
object TemplateIndexer extends App {
  val logger = LoggerFactory.getLogger(this.getClass)
  override def main(args: Array[String]) {
     val groupSize = 100000
     val solrUrl = args(0)
     val server = new ConcurrentUpdateSolrServer(solrUrl, 1000, 4)
     val indexer = new TemplateIndexer(server)
     val lines = Source.fromInputStream(System.in, "UTF8").getLines
     val groups = lines.grouped(groupSize)
     for (group <- groups; line <- group.par) indexer.indexLine(line)
     server.commit()
  }
} 
