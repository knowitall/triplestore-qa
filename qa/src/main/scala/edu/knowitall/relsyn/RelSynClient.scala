package edu.knowitall.relsyn

import com.typesafe.config.ConfigFactory
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.common.SolrDocument
import org.apache.solr.client.solrj.SolrQuery
import scala.collection.JavaConversions._
import org.apache.solr.client.solrj.SolrQuery.SortClause
import edu.knowitall.util.MathUtils
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.postag.Postagger
import org.slf4j.LoggerFactory
import edu.knowitall.util.NlpTools

case class RelSynClient(url: String = RelSynClient.defaultUrl, 
    					stemmer: Stemmer = NlpTools.stemmer,
    					tokenizer: Tokenizer = NlpTools.tokenizer,
    					tagger: Postagger = NlpTools.tagger,
    					maxHits: Int = RelSynClient.defaultMaxHits,
    					scale: Boolean = RelSynClient.defaultScale) {
  
  private val client = new HttpSolrServer(url)
  val logger = LoggerFactory.getLogger(this.getClass)

  
  private def getValue(n: String, d: SolrDocument): Option[Any] = {
    val value = d.getFieldValue(n)
    if (value == null) None else Some(value)
  }
  
  private def getString(n: String, d: SolrDocument) = for {
    v <- getValue(n, d)
    s <- v match {
      case s: String => Some(s)
      case _ => None
    }
  } yield s
  
  private def getDouble(n: String, d: SolrDocument) = for {
    v <- getValue(n, d)
    d <- v match {
      case d: Double => Some(d)
      case f: Float => Some(f.toDouble)
      case _ => None
    }
  } yield d
  
  private def getBoolean(n: String, d: SolrDocument) = for {
    v <- getValue(n, d)
    b <- v match {
      case b: Boolean => Some(b)
      case _ => None
    }
  } yield b
  
  private def fromDoc(d: SolrDocument) = for {
    rel1 <- getString("rel1", d)
    rel2 <- getString("rel2", d)
    inverted <- getBoolean("inverted", d)
    count1 <- getDouble("marg_count1", d)
    count2 <- getDouble("marg_count2", d)
    joint <- getDouble("joint_count", d)
    pmi <- getDouble("pmi", d)
  } yield RelSynRule(rel1, rel2, inverted, count1, count2, joint, pmi)
  
  private def stemString(s: String) = {
    val tokens = tokenizer(s)
    val tagged = tagger.postagTokenized(tokens)
    val result = tagged.map {
      t => MorphaStemmer.lemmatizePostaggedToken(t).lemma.toLowerCase() 
    }
    result.mkString(" ")
  }
  
  def relSyns(s: String, limit: Int = maxHits) = {
    val stems = stemString(s)
    val query = new SolrQuery(s"""${RelSynClient.searchField}:"${stems}"""")
    logger.debug(s"Getting relSyns for ${stems}")
    query.setRows(maxHits)
    query.addSort(new SortClause("pmi", SolrQuery.ORDER.desc))
    val resp = client.query(query)
    val pairs = resp.getResults().toList.flatMap(fromDoc)
    pairs.map(pair => pair.copy(pmi = scalePmi(pair.pmi)))
    
  }
  
  def relSyns(c: TConjunct): List[RelSynRule] = c.values.get(Search.rel) match {
    case Some(UnquotedTLiteral(l)) => relSyns(l)
    case Some(QuotedTLiteral(l)) => relSyns(l)
    case _ => Nil
  }
    
  private def scalePmi(x: Double): Double = 
    if (scale) MathUtils.clipScale(x, RelSynClient.minPmi, RelSynClient.maxPmi)
    else x
  
}

case object RelSynClient {
  val conf = ConfigFactory.load()
  val defaultUrl = conf.getString("relsyn.url")
  val defaultMaxHits = conf.getInt("relsyn.maxHits")
  val defaultScale = conf.getBoolean("relsyn.scale")
  val minPmi = conf.getDouble("relsyn.minPmi")
  val maxPmi = conf.getDouble("relsyn.maxPmi")
  val searchField = "rel1_exact"
}