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
    sq.setRows(hits)//.setFields("arg1", "rel", "arg2", "id", "namespace")
  }
  
  def fieldNames(doc: SolrDocument): List[String] = {
    doc.getFieldNames().toList.map { x => x.toString() }
  }
  
  type Value = Any
  type Attr = String
  
  def toTupleValue(v: Any): Option[Value] = {
    v match {
      case v: String => Some(v)
      case v: Float => Some(v)
      case v: Double => Some(v)
      case v: Integer => Some(v)
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
  
  import Conditions._ 
  import Search._
  import Field._
  
  type Tuples = Iterable[Tuple]
  type TuplePred = Tuple => Boolean
  type TupleMap = Tuple => Tuple
 
  def ExecQuery(n: String, q: Query, hits: Int = 10) = client.namedSearch(n, q)
  def SearchFor(s: String, q: Query*) = ExecQuery(s, Conjunction(q:_*))
  def PartialSearchFor(n: String, q: Query*): PartialSearcher = {
    PartialSearcher(Conjunction(q:_*), ExecQuery(n, _)) 
  }
  def ProjectOn(s: String, ts: Tuples) = Operators.Project(On(s))(ts)
  def Project(m: TupleMap, ts: Tuples) = Operators.Project(m)(ts)
  def Join(cond: TuplePred, ts1: Tuples, ts2: Tuples) = Operators.NestedLoopJoin(cond)(ts1, ts2)
  
  def SearchJoin(a1: String, a2: String, ts: Tuples, q: PartialSearcher): Tuples = {
    val cond = AttrsSim(a1, a2, 0.95)
    PartialSearchJoin(cond)(ts, q)
  }

  val Arg1Eq = (v: String) => FieldPhrase(arg1_exact, v)
  val Arg2Eq = (v: String) => FieldPhrase(arg2_exact, v)
  val RelEq = (v: String) => FieldPhrase(rel_exact, v)
 
  val Arg1Cont = (v: String) => FieldKeywords(arg1, v)
  val Arg2Cont = (v: String) => FieldKeywords(arg2, v)
  val RelCont = (v: String) => FieldKeywords(rel, v)

}