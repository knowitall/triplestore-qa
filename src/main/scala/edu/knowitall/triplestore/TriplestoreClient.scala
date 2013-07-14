package edu.knowitall.triplestore
import scala.collection.JavaConverters._
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.util.ClientUtils
import org.apache.solr.client.solrj.SolrQuery
import scala.collection.JavaConversions._
import org.apache.solr.common.SolrDocument
import java.util.ArrayList
import Search.TSQuery

/**
 * The interface to a Triplestore.
 */
trait TriplestoreClient {
  
  /**
   * Counts the number of triples that match the given query.
   */
  def count(q: TSQuery): Long
  
  /**
   * Searches and returns the results as Tuple objects.
   */
  def search(q: TSQuery): List[Tuple]
    
  /**
   * Searches and returns Tuple objects, but adds the prefix
   * "$name." to all of the attributes.
   */
  def namedSearch(name: String, q: TSQuery): List[Tuple] = {
    search(q) map { t => t.renamePrefix(name)}
  }
}

/**
 * This class is used to query a Solr server and return Tuple objects. The
 * URL should point to the Solr instance. "hits" is the default number of hits
 * that is returned by the search.
 */
case class SolrClient(url: String, hits: Int = 10) extends TriplestoreClient {
  
  val server = new HttpSolrServer(url)
  
  def escape(s: String): String = ClientUtils.escapeQueryChars(s)
  
  /**
   * Takes a Search.Query object and maps it to a SolrQuery object.
   */
  def buildQuery(q: TSQuery): SolrQuery = {
    val sq = new SolrQuery(q.toQueryString)
    System.err.println(s"Issuing Solr Query: ${sq.getQuery}")
    sq.setRows(hits)
  }
  
  /**
   * Builds a SolrQuery object used to count the number of hits returned
   * by the given Search.Query object. Returns 0 rows.
   */
  def buildCountQuery(q: TSQuery): SolrQuery = {
    val sq = new SolrQuery(q.toQueryString)
    sq.setRows(0)
  }
  
  /**
   * The string field names of the given solr document.
   */
  def fieldNames(doc: SolrDocument): List[String] = {
    doc.getFieldNames().toList.map { x => x.toString() }
  }
  
  // Mnemonics used to remember what the attributes/values of a Tuple are.
  type Value = Any
  type Attr = String
  
  /**
   * Used to convert the values of a Solr field into Values for a Tuple.
   */
  def toTupleValue(v: Any): Option[Value] = {
    v match {
      case v: String => Some(v)
      case v: Float => Some(v)
      case v: Double => Some(v)
      case v: Integer => Some(v)
      case _ => None
    }
  }
  
  /**
   * Gets the (attribute, value) pairs from the given Solr doc.
   */
  def docToFields(doc: SolrDocument): List[(Attr, Value)] = {
    for (name <- doc.getFieldNames().toList;
        value = doc.getFieldValue(name);
        tvalue <- toTupleValue(value)) 
      yield (name, tvalue)
  }
  
  /**
   * Converts a Solr document to a Tuple object.
   */
  def docToTuple(doc: SolrDocument): Tuple = Tuple(docToFields(doc).toMap)

  /**
   * Returns the number of documents in Solr that match the given query.
   */
  def count(q: TSQuery): Long = {
    System.err.println("Counting..." + q)
    val query = buildCountQuery(q)
    val resp = server.query(query)
    resp.getResults().getNumFound()
  }
  
  /**
   * Searches Solr and returns Tuple objects.
   */
  def search(q: TSQuery): List[Tuple] ={
    val query = buildQuery(q)
    query.setRows(hits)
    System.err.println(q)
    val resp = server.query(query)
    resp.getResults().toList.map(docToTuple)
  }
  
}

/**
 * This is a utility class used to instantiate some common relational operations
 * and shortcuts. Builds on a TriplestoreClient, which is used to interact
 * with the underlying Solr instance.
 */
case class TriplestorePlan(client: TriplestoreClient) {
  
  import Conditions._ 
  import Search._
  import Field._
  
  // Mnemonics
  type Tuples = Iterable[Tuple]
  type TuplePred = Tuple => Boolean
  type TupleMap = Tuple => Tuple
 
  /**
   * Shortcut for executing the given query with the given name.
   */
  def ExecQuery(n: String, q: TSQuery) = client.namedSearch(n, q)
  
  /**
   * Searches for the conjunction of the given queries, naming them with
   * the given name.
   */
  def SearchFor(s: String, q: TSQuery*) = ExecQuery(s, Conjunction(q:_*))
  
  /**
   * This is a partial search object, which is used in join algorithms. It's
   * a way to represent a query that has yet to be executed against Solr that
   * will be joined with another set of tuples.
   */
  def PartialSearchFor(n: String, q: TSQuery*): PartialSearcher = {
    PartialSearcher(Conjunction(q:_*), ExecQuery(n, _)) 
  }
  
  /**
   * A shortcut function for projecting the given tuples on a single attribute.
   */
  def ProjectOn(s: String, ts: Tuples) = Operators.Project(On(s))(ts)
  
  /**
   * A shortcut function for projecting the given tuples using the given 
   * map Tuple => Tuple.
   */
  def Project(m: TupleMap, ts: Tuples) = Operators.Project(m)(ts)
  
  /**
   * A shortcut for doing a nested loop join on the given pair of tuple 
   * iterables.
   */
  def Join(cond: TuplePred, ts1: Tuples, ts2: Tuples) = 
    Operators.NestedLoopJoin(cond)(ts1, ts2)

  /**
   * A shortcut function for performing a "search join". A search join is used
   * when joining two sets of tuples together, but where one tuple set is too
   * large to fetch from Solr. Instead, this algorithm takes a smaller set of
   * tuples (in memory) and iteratively makes a query for each row in the 
   * table. This essentially trades off making one large query (possibly 
   * exhausting memory) with making many smaller ones.
   * 
   * For example, a SearchJoin can be used when joining "($x, type, us 
   * president)" which is relatively small with "($x, type, lawyer)" which is 
   * relatively big. The algorithm will enumerate all $x from the first table
   * and then substitute it in the second query. 
   */
  def SearchJoin(a1: String, a2: String, ts: Tuples, 
      q: PartialSearcher): Tuples = {
    // Joins using string similarity instead of strict equality
    val cond = AttrsSim(a1, a2, 0.9) 
    PartialSearchJoin(cond)(ts, q)
  }

}
