package edu.knowitall.triplestore

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution._
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.core.CoreContainer
import java.io.File
import org.apache.commons.io.IOUtils
import scala.io.Source
import java.io._
import org.apache.solr.common.SolrInputDocument

/**
 * Creates a temporary triplestore solr server that can be used for testing.
 * It will look on the classpath for the files necessary to create a solr 
 * instance (schema.xml, solr.xml, solrconf.xml, etc.). It then looks for the
 * file triples.txt and adds them to the index. 
 * 
 * This object will create temporary files to store the index.
 */
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
  
  def lineToDoc(line: String): Option[SolrInputDocument] = {
    val fields = line.trim().split("\t")
    if (fields.size == 3) {
      val arg1 = fields(0)
      val rel = fields(1)
      val arg2 = fields(2)
      val doc = new SolrInputDocument()
      doc.addField("arg1", arg1)
      doc.addField("rel", rel)
      doc.addField("arg2", arg2)
      doc.addField("namespace", "test")
      Some(doc)
    } else {
      None
    }
  }
  
  def addDocs(in: InputStream, server: EmbeddedSolrServer) = {
    val lines = Source.fromInputStream(in).getLines
    for ((line, i) <- lines.zipWithIndex;
         doc <- lineToDoc(line)) {
      doc.addField("id", i.toString)
      server.add(doc)
    }
    server.commit()
  }

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
  val server = new EmbeddedSolrServer(coreContainer, "collection1")
  
  
  // Add the documents
  addDocs(resource("triples.txt"), server)
    
  def getServer = server

  def shutdown = coreContainer.shutdown()
  
}

/**
 * A mock triplestore client that is backed by the mock Solr server.
 */
case class MockTriplestoreClient() extends TriplestoreClient {

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