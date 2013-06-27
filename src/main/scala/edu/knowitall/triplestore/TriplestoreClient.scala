package edu.knowitall.triplestore
import scala.collection.JavaConverters._
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.util.ClientUtils
import org.apache.solr.client.solrj.SolrQuery
import scala.collection.JavaConversions._
import org.apache.solr.common.SolrDocument
import java.util.ArrayList
import Search.Query


case class TriplestoreClient(url: String, hits: Int = 10) {
  
  val server = new HttpSolrServer(url)
  
  def escape(s: String): String = ClientUtils.escapeQueryChars(s)
  
  def buildQuery(q: Query): SolrQuery = {
    val sq = new SolrQuery(q.toQueryString)
    sq.setRows(hits).setFields("arg1", "rel", "arg2", "id", "namespace")
  }
  
  def fieldNames(doc: SolrDocument): List[String] = {
    doc.getFieldNames().toList.map { x => x.toString() }
  }
  
  type Value = List[String]
  type Attr = String
  
  def toTupleValue(v: Any): Option[Value] = {
    v match {
      case v: ArrayList[_] => Some(v.asScala.toList.map(x => x.toString()))
      case v: Any => Some(List(v.toString()))
      case _ => None
    }
  }
  
  def docToFields(doc: SolrDocument): List[(Attr, Value)] = {
    for (name <- doc.getFieldNames().toList;
        value = doc.getFieldValue(name);
        tvalue <- toTupleValue(value)) 
      yield (name, tvalue)
  }
  
  def docToTuple(doc: SolrDocument): Tuple = Tuple(docToFields(doc).toMap)

  
  def search(q: Query): List[Tuple] ={
    val query = buildQuery(q)
    query.setRows(hits)
    val resp = server.query(query)
    resp.getResults().toList.map(docToTuple)
  }
  
  def namedSearch(name: String, q: Query): List[Tuple] = {
    search(q) map { t => t.renamePrefix(name)}
  }
  

}

/*
object Test extends Application {
  val c = TriplestoreClient("http://rv-n12:8983/solr/triplestore")
  val Search = c.Search
  val rel1 = Search("isPrez", rel = "grown in", arg2 = "africa")
  val rel2 = Search("buried", rel = "Type", arg2 = "fruit", namespace="freebase")
  val cond = Conditions.Intersects[String]("isPrez.arg1", "buried.arg1")
  val rel3 = Operators.Join(cond)(rel1, rel2)
  val rel4 = Operators.Project(List("isPrez.arg1"))(rel3)
  rel4 map { x => println(x.getOrElse("isPrez.arg1", List[String]())) }
}*/