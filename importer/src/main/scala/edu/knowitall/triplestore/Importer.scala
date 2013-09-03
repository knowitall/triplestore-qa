package edu.knowitall.triplestore
import scala.collection.mutable.HashMap
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.common.SolrInputDocument
import org.slf4j.LoggerFactory
import edu.knowitall.openie.models.{ ExtractionArgument, ExtractionGroup, ReVerbExtraction, ReVerbExtractionGroup, Instance }
import edu.knowitall.openie.models.ExtractionCluster
import edu.knowitall.openie.models.Extraction
import scopt.OptionParser
import scala.util.Success
import scala.util.Failure

object Importer {

  case class Config(
    val format: TupleFormat = PlainTextFormat(),
    val solrUrl: String = "",
    val namespace: String = "",
    val idPrefix: String = "")

  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("Importer") {

      arg[String]("solr-url") action { (str: String, c: Config) => c.copy(solrUrl = str) }

      arg[String]("format") action {
        (str: String, c: Config) =>
          str match {
            case "plain" => c.copy(format = PlainTextFormat())
            case "reverb" => c.copy(format = GroupedReVerbFormat())
            case "openie4" => c.copy(format = ClusteredOpenIE4Format())
            case "probase" => c.copy(format = ProbaseFormat())
          }
      } text ("format (plain|reverb|openie4|probase)")

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
    var idPrefix = config.idPrefix
    val solr = new ConcurrentUpdateSolrServer(url, 1000, 4)

    if (idPrefix == "") {
      idPrefix = namespace
    }

    var count = 0;
    var start = System.currentTimeMillis()
    for (doc <- config.format.docIterator) {

      if (doc.getFieldValue("id") == null) {
        doc.addField("id", idPrefix + count)
      }

      if (doc.getFieldValue("namespace") == null) {
        doc.addField("namespace", namespace)
      }

      solr.add(doc)
      count += 1
      if (count % 10000 == 0) {
        System.err.print(".")
      }
    }
    solr.blockUntilFinished()
    solr.commit()
    var runTime = (System.currentTimeMillis() - start) / 1000
    System.err.println()
    System.err.println("Added " + count + " docs in " + runTime + "secs")
    solr.shutdownNow()
  }
}