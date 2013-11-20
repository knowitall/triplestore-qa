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

case class ParaphraseTemplateClient(solrUrl: String, maxHits: Int, scale: Boolean = ParaphraseTemplateClient.scale) {
  
  def this() = this(ParaphraseTemplateClient.defaultUrl, ParaphraseTemplateClient.defaultMaxHits, ParaphraseTemplateClient.scale)
  
  val logger = LoggerFactory.getLogger(this.getClass)
  val server = new HttpSolrServer(solrUrl)
  val searchField = "template1_exact"
  def paraphrases(s: String, limit: Int = maxHits) = {
    val query = new SolrQuery(s"""${searchField}:"${s}"""")
    query.setRows(maxHits)
    query.addSort(new SortClause("pmi", SolrQuery.ORDER.desc))
    logger.info(s"Sending query: ${query.toString()}")
    val resp = server.query(query)
    logger.info(s"Found ${resp.getResults().getNumFound()} hits")
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
}

case class TemplatePair(template1: String, template2: String, pmi: Double, count1: Double, count2: Double, jointCount: Double) extends QaAction {
  def this(t1: String, t2: String, j: Double, c1: Double, c2: Double) = this(t1, t2, TemplatePair.pmi(j, c1, c2), c1, c2, j)
  def this(t1: String, t2: String, j: String, c1: String, c2: String) = this(t1, t2, j.toDouble, c1.toDouble, c2.toDouble)
}

case object TemplatePair {
  
  def pmi(j: Double, m1: Double, m2: Double): Double = Math.log(j) - Math.log(m1) - Math.log(m2) 
  
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

class TemplateIndexer(server: SolrServer) {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def pairToDoc(pair: TemplatePair): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("template1", pair.template1, pair.pmi.toFloat)
    doc.addField("template2", pair.template2, pair.pmi.toFloat)
    doc.addField("pmi", pair.pmi)
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
