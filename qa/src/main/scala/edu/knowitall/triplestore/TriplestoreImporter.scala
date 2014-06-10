package edu.knowitall.triplestore

import scopt.OptionParser
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer

object TriplestoreImporter {

  def lineToPairs(line: String): Option[List[(String, String)]] = {
    val fields = line.split("\t")
    if (fields.size % 2 == 0) {
      val pairs = fields.grouped(2).map {
        case Array(a, b) => (a, b)
      }.toList
      Some(pairs)
    } else {
      System.err.println(s"Invalid format for line: '$line'")
      None
    }
  }

  def recordToSolr(rec: List[(String, Any)]): SolrInputDocument = {
    val doc = new SolrInputDocument()
    for ((k, v) <- rec) {
      doc.addField(k, v)
    }
    return doc
  }

  case class Config(solrUrl: String = "", numThreads: Int = 1,
      queueSize: Int = 1000)

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("TriplestoreImporter") {
      arg[String]("solr-url") action { (str: String, c: Config) => c.copy(solrUrl = str) }
      arg[Int]("num-threads") action { (i: Int, c: Config) => c.copy(numThreads = i) }
      arg[Int]("queue-size") action { (i: Int, c: Config) => c.copy(queueSize = i) }
    }
    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None =>
    }
  }
  
  def run(config: Config) {

    val solr = new ConcurrentUpdateSolrServer(config.solrUrl, config.queueSize, config.numThreads)

    val lines = io.Source.fromInputStream(System.in, "UTF8").getLines
    val pairs = lines flatMap lineToPairs
    val docs = pairs map recordToSolr

    var numAdded = 0
    val start = System.currentTimeMillis()
    for ((doc, i) <- docs.zipWithIndex) {
      val resp = solr.add(doc)
      if (resp.getStatus() != 0) {
        System.err.println(s"Could not add '$doc'")
        System.err.println(resp.getResponse.toString)
      } else {
        numAdded += 1
        if (numAdded % 10000 == 0) {
          val elapsed = (System.currentTimeMillis() - start) / 1000
          System.err.println(s"Added $numAdded documents in $elapsed seconds")
        }
      }
    }
    solr.blockUntilFinished()
    solr.commit()
    val elapsed = (System.currentTimeMillis() - start) / 1000
    System.err.println(s"Added $numAdded docs in $elapsed seconds")
    solr.shutdownNow()
  }



}
