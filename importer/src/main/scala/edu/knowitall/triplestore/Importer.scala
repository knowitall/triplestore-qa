package edu.knowitall.triplestore
import scala.collection.mutable.HashMap
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.common.SolrInputDocument
import org.slf4j.LoggerFactory
import edu.knowitall.openie.models.{ ExtractionArgument, ExtractionGroup, ReVerbExtraction, ReVerbExtractionGroup, Instance }
import edu.knowitall.openie.models.ExtractionCluster
import edu.knowitall.openie.models.Extraction
import scopt.OptionParser
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.atomic.AtomicLong

object Importer {

  case class Config(
    val format: String = "plain",
    val solrUrl: String = "",
    val namespace: String = "",
    val idPrefix: String = "")

  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("Importer") {

      arg[String]("solr-url") action { (str: String, c: Config) => c.copy(solrUrl = str) }

      arg[String]("format") action { (str: String, c: Config) => c.copy(format = str) } text ("format (plain|reverb|openie4|probase)")

      opt[String]('n', "namespace") optional () action {
        (str: String, c: Config) => c.copy(namespace = str)
      } text ("default namespace")

      opt[String]('p', "idprefix") optional () action {
        (str: String, c: Config) => c.copy(idPrefix = str)
      }

    }
    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def run(config: Config) = {
    val url = config.solrUrl
    val namespace = config.namespace
    val idPrefix = if (config.idPrefix.nonEmpty) config.idPrefix else namespace
    val solr = new ConcurrentUpdateSolrServer(url, 1000, 8)
    //val solr = new HttpSolrServer(url)
    
    val id = new AtomicLong(0)
    def getNextId() =  idPrefix + id.getAndIncrement()
    val idFactory = getNextId _

    val format = config.format match {
      case "plain" =>   PlainTextFormat(config.namespace, idFactory)
      case "triples" =>   PlainTextTripleFormat(config.namespace, idFactory)
      case "reverb" =>  GroupedReVerbFormat(config.namespace, idFactory)
      case "openie4" => ClusteredOpenIE4Format(config.namespace, idFactory)
      case "probase" => ProbaseFormat(config.namespace, idFactory)
      case _ => throw new IllegalArgumentException("Invalid format: " + config.format)
    }


    var start = System.currentTimeMillis()
    for (doc <- format.docIterator) {

      solr.add(doc)
      if (id.get % 10000 == 0) {
        System.err.print(".")
      }
    }
    solr.blockUntilFinished()
    solr.commit()
    var runTime = (System.currentTimeMillis() - start) / 1000
    System.err.println()
    System.err.println("Added " + id.get + " docs in " + runTime + "secs")
    solr.shutdownNow()
  }
}
