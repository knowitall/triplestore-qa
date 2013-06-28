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

case class TriplestorePlan(client: TriplestoreClient) {
  import Operators._
  import Conditions._ 
  import Search._
  import Field._
  
  type Tuples = Iterable[Tuple]
  type TuplePred = Tuple => Boolean
 
  def ExecQuery(n: String, q: Query, hits: Int = 10) = client.namedSearch(n, q)
  def SearchFor(s: String, q: Query*) = ExecQuery(s, Conjunction(q:_*))
  def PartialSearchFor(n: String, q: Query*): PartialSearcher = {
    PartialSearcher(Conjunction(q:_*), ExecQuery(n, _)) 
  }
  def ProjectOn(s: String, ts: Tuples) = Project(On(s))(ts)
  def Join(cond: TuplePred, ts1: Tuples, ts2: Tuples) = NestedLoopJoin(cond)(ts1, ts2)
  

 
  val Arg1 = (v: String) => FieldEquals(arg1, v)
  val Arg2 = (v: String) => FieldEquals(arg2, v)
  val Rel = (v: String) => FieldEquals(rel, v)

}