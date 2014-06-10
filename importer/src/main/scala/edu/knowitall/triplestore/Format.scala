package edu.knowitall.triplestore

import org.apache.solr.common.SolrInputDocument
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
import edu.knowitall.browser.solr.SolrDocumentConverter

trait Format {
  
  def namespace: String
  
  val idFactory: () => String

  def docIterator(): Iterator[SolrInputDocument]
}

abstract class TupleFormat extends Format {

  protected def groupIterator(): Iterator[List[(String, Any)]]

  def close(): Unit

  private def recordToSolr(rec: List[(String, Any)]): SolrInputDocument = {
    val doc = new SolrInputDocument()
    for ((k, v) <- rec) {
      doc.addField(k, v)
    }

    if (doc.getFieldValue("id") == null) {
      doc.addField("id", idFactory())
    }

    if (doc.getFieldValue("namespace") == null) {
      doc.addField("namespace", namespace)
    }
    
    return doc
  }

  def docIterator(): Iterator[SolrInputDocument] = groupIterator map recordToSolr
}

case class ProbaseFormat(namespace: String, idFactory: () => String) extends TupleFormat {

  val relationName = "Instance Of"

  val splitRegex = "\t".r

  def close() = ()

  def groupIterator() = {
    io.Source.fromInputStream(System.in, "UTF8").getLines flatMap lineToPairs
  }

  val attrNames = List("arg2",
    "arg1",
    "freq_i",
    "popularity_i",
    "conceptFrequency_i",
    "conceptSize_i",
    "conceptVagueness_f",
    "zipfSlope_f",
    "zipfPearsonCoefficient_f",
    "entityFrequency_i",
    "entitySize_i")

  def lineToPairs(line: String): Option[List[(String, String)]] = {
    val split = splitRegex.split(line)
    if (split.length >= attrNames.length) {
      val list = ("rel", relationName) :: attrNames.zip(split)
      Some(list filter { case (a,b) => b != "NULL" })
    } else {
      System.err.println(s"Warning, unparseable probase entry $line")
      None
    }
  }
}

case class PlainTextFormat(namespace: String, idFactory: () => String) extends TupleFormat {

  case class Triple(arg1: String, rel: String, arg2: String)

  def groupIterator() = {
    io.Source.fromInputStream(System.in, "UTF8").getLines flatMap lineToPairs
  }

  def close() {}

  private var last: Option[Triple] = None

  def lineToPairs(line: String): Option[List[(String, String)]] = {
    val kvPairs = {
      line.split("\t").grouped(2).map {
        case Array(a, b) => (a, b)
        case x: Array[_] => throw new IllegalStateException(x.mkString(", "))
      }.toList
    }
    Some(kvPairs)
  }
}

case class PlainTextTripleFormat(namespace: String, idFactory: () => String) extends TupleFormat {

  case class Triple(arg1: String, rel: String, arg2: String)

  def groupIterator() = {
    io.Source.fromInputStream(System.in, "UTF8").getLines flatMap lineToPairs
  }

  def close() {}

  private var last: Option[Triple] = None

  def lineToPairs(line: String): Option[List[(String, String)]] = {
    val kvPairs = {
      line.split("\t").grouped(2).map {
        case Array(a, b) => (a, b)
      }.toList
    }
    val kvMap = kvPairs.toMap
    require(kvMap.contains("arg1") && kvMap.contains("arg2") && kvMap.contains("rel"))

    val thisTriple = Some(Triple(kvMap("arg1"), kvMap("rel"), kvMap("arg2")))
    if (thisTriple.equals(last)) None else {
      last = thisTriple
      Some(kvPairs)
    }
  }
}

case class ClusteredOpenIE4Format(namespace: String, idFactory: () => String) extends Format {

  def close() = ()

  private def clusterIterator() = {
    val source = io.Source.fromInputStream(System.in, "UTF8")
    source.getLines.grouped(1000).flatMap { group =>
      group.par flatMap { l =>
        val tryRead = ExtractionCluster.formatter.read(l)
        tryRead match {
          case Success(cluster) => Some(cluster)
          case Failure(ex) => { ex.printStackTrace(); None }
        }
      }
    }
  }

  def docIterator() = clusterIterator().flatMap(c => SolrDocumentConverter.toSolrDocuments(c, idFactory))
}

case class GroupedReVerbFormat(namespace: String, idFactory: () => String) extends TupleFormat {

  type REG = ExtractionGroup[ReVerbExtraction]
  type RINST = Instance[ReVerbExtraction]

  def groupIterator() = {
    io.Source.fromInputStream(System.in, "UTF8").getLines.grouped(1000).flatMap { group =>
      group.par.map(l => groupToRecord(ReVerbExtractionGroup.deserializeFromString(l).get))
    }
  }

  def close() {}

  def groupToRecord(reg: REG): List[(String, Any)] = {
    var lst = List.empty[(String, Any)]
    val maxConfInst = reg.instances.maxBy(_.confidence)
    lst = ("arg1", getArg1(maxConfInst)) :: lst
    lst = ("rel", getRel(maxConfInst)) :: lst
    lst = ("arg2", getArg2(maxConfInst)) :: lst
    lst = lst ++ getOptionals(reg)
    return lst
  }

  def getArg1(inst: RINST): String = inst.extraction.arg1Text

  def getArg2(inst: RINST): String = inst.extraction.arg2Text

  def getRel(inst: RINST): String = inst.extraction.relText

  def getOptionals(reg: REG): List[(String, Any)] = {
    var lst = List.empty[(String, Any)]
    lst = lst ++ argOptionals("arg1", reg.arg1) ++ argOptionals("arg2", reg.arg2)
    for (corp <- reg.corpora) {
      lst = ("corpora_ss", corp) :: lst
    }
    lst = ("num_extrs_i", reg.instances.size) :: lst
    val confs = reg.instances.map(i => i.confidence).toList
    val maxConf = confs match { case x :: xs => confs.max case _ => 0.0 }
    lst = ("conf_f", maxConf) :: lst
    return lst
  }

  def argOptionals(name: String, arg: ExtractionArgument): List[(String, String)] = {
    var lst = List.empty[(String, String)]
    if (arg.hasEntity) {
      val fbid = arg.entity.get.fbid
      lst = (name + "_fbid_s", fbid) :: lst
    }
    return lst
  }
}
