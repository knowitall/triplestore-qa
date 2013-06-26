package edu.knowitall.triplestore
import scala.collection.JavaConverters._
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.util.ClientUtils
import org.apache.solr.client.solrj.SolrQuery
import scala.collection.JavaConversions._
import org.apache.solr.common.SolrDocument
import java.util.ArrayList



case class TriplestoreClient(url: String, hits: Int = 10000) {
  
  val server = new HttpSolrServer(url)
  
  def escape(s: String): String = ClientUtils.escapeQueryChars(s)
  
  def buildQuery(params: List[(String, String)]): SolrQuery = {
    val escaped = params filter { case (a, b) => a != "" && b != "" } map { case (a, b) => escape(a) + ":\"" + escape(b) +"\"" }
    new SolrQuery(escaped.mkString(" AND "))
  }
  
  def docToTuple(doc: SolrDocument): Tuple = {
    val names = doc.getFieldNames().toList
    val values = names.map { n => doc.getFieldValue(n) }
    val values2 = values.map{ 
      case x: ArrayList[_] => x.asScala.toList
      case x => x
    }
    Tuple(names, values2)
  }
  
  def search(args: List[(String, String)]): List[Tuple] ={
    val query = buildQuery(args)
    query.setRows(hits)
    val resp = server.query(query)
    resp.getResults().toList.map(docToTuple)
  }
  
  def namedSearch(name: String, args: List[(String, String)]): List[Tuple] = {
    search(args) map { t => t.renamePrefix(name)}
  }
  
  case class Search(name: String, arg1: String = "", rel: String = "", arg2: String = "", namespace: String = "") extends Iterable[Tuple] {
    val args = List(("arg1", arg1), ("arg2", arg2), ("rel", rel), ("namespace", namespace))
    val hits = namedSearch(name, args)
    override def iterator: Iterator[Tuple] = hits.iterator
  }
  

}

object Test extends Application {
  val c = TriplestoreClient("http://rv-n12:8983/solr/triplestore")
  val Search = c.Search
  val rel1 = Search("isPrez", rel = "grown in", arg2 = "africa")
  val rel2 = Search("buried", rel = "Type", arg2 = "fruit", namespace="freebase")
  val cond = Conditions.Intersects[String]("isPrez.arg1", "buried.arg1")
  val rel3 = Operators.Join(cond)(rel1, rel2)
  val rel4 = Operators.Project(List("isPrez.arg1"))(rel3)
  rel4 map { x => println(x.getOrElse("isPrez.arg1", List[String]())) }
}