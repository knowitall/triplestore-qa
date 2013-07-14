package edu.knowitall.triplestore

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import edu.knowitall.triplestore.Search.TSQuery
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.core.CoreContainer
import java.io.File
import org.apache.commons.io.IOUtils
import java.io._

object MockSolrServer {
  
  def resource(name: String) = {
    val stream = getClass.getResourceAsStream(name)
    if (stream == null)
      throw new IllegalArgumentException(s"Could not find resource: $name")
    stream
  }
  
  def outstream(dir: File, name: String) = 
    new FileOutputStream(new File(dir, name))
  
  def copy(from: String, dir: File) = 
    IOUtils.copy(resource(from), outstream(dir, from))
  

  // Create the temp directory for the server
  val solrHome = File.createTempFile("solr", null)
  solrHome.delete()
  solrHome.mkdir()
  
  // Create the collections and conf directories
  val coll1 = new File(solrHome, "collection1")
  val conf = new File(coll1, "conf")
  conf.mkdirs()
  
  // Copy the solr.xml file
  copy("solr.xml", solrHome)
  
  // Copy the configuration files
  List("solrconfig.xml", "schema.xml", "stopwords.txt", "synonyms.txt", 
      "protwords.txt", "currency.xml") foreach(copy(_, conf))
      
  // Create the server
  System.setProperty("solr.solr.home", solrHome.getAbsolutePath())
  val initializer = new CoreContainer.Initializer()
  val coreContainer = initializer.initialize()
    
  def getServer = new EmbeddedSolrServer(coreContainer, "collection1")

  def shutdown = coreContainer.shutdown()
  
}

class MockTriplestoreClient extends TriplestoreClient {

  val server = MockSolrServer.getServer
  
  val hits = 100
  
  def count(query: TSQuery): Long = 
    server.query(SolrClient.buildQuery(query)).getResults().getNumFound()
  
  def search(q: TSQuery): List[Tuple] = {
    val query = SolrClient.buildQuery(q)
    query.setRows(hits)
    System.err.println(query)
    val resp = server.query(query)
    resp.getResults().toList.map(SolrClient.docToTuple)
  }

}