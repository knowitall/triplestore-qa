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
  
  sealed abstract class Format {
    def groupIterator(): Iterator[List[(String, Any)]]
    def close(): Unit
  }
  
  case class ProbaseFormat() extends Format {
    
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
        Some(list)
      }
      else {
        System.err.println(s"Warning, unparseable probase entry $line")
        None
      }
    }
  }
  

  case class PlainTextFormat() extends Format {

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
 
  case class ClusteredOpenIE4Format() extends Format {
    
    def close() = ()
    
    def groupIterator() = {
      val source = io.Source.fromInputStream(System.in, "UTF8")
      source.getLines.grouped(1000).flatMap { group =>
        group.par flatMap { l => 
          val tryRead = ExtractionCluster.formatter.read(l)
          tryRead match {
            case Success(cluster) => Some(clusterToRecord(cluster))
            case Failure(ex) => { ex.printStackTrace(); None }
          }
        } 
      }
    }
   
    def clusterToRecord(cluster: ExtractionCluster[Extraction]): List[(String, Any)] = {
      var lst = List.empty[(String, Any)]
      val maxConfInst = cluster.instances.maxBy(_.confidence)
      lst = ("arg1", getArg1(maxConfInst)) :: lst
      lst = ("rel", getRel(maxConfInst)) :: lst
      lst = ("arg2", getArg2(maxConfInst)) :: lst
      lst = lst ++ getOptionals(cluster)
      return lst
    }
    
    def getArg1(inst: Extraction): String = inst.arg1Text
    
    def getArg2(inst: Extraction): String = inst.arg2Text
    
    def getRel(inst: Extraction): String = inst.relText
    
    def getOptionals(cluster: ExtractionCluster[Extraction]): List[(String, Any)] = {
      var lst = List.empty[(String, Any)]
      lst = lst ++ argOptionals("arg1", cluster.arg1) ++ argOptionals("arg2", cluster.arg2)
      for (corp <- cluster.corpora) {
        lst = ("corpora_ss", corp) :: lst
      }
      lst = ("num_extrs_i", cluster.instances.size) :: lst
      val confs = cluster.instances.map(i => i.confidence).toList
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
  
  case class GroupedReVerbFormat() extends Format {
    
    type REG = ExtractionGroup[ReVerbExtraction]
    type RINST = Instance[ReVerbExtraction]
    
    def groupIterator() = {
      io.Source.fromInputStream(System.in, "UTF8").getLines.grouped(1000).flatMap { group =>
        group.par.map(l => groupToRecord(ReVerbExtractionGroup.deserializeFromString(l).get))
      }
    }
    
    def close () {}
    
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

  case class Config(
      val format: Format = PlainTextFormat(), 
      val solrUrl: String = "",
      val namespace: String = "",
      val idPrefix: String = "")
      
  def recordToSolr(rec: List[(String, Any)]): SolrInputDocument = {
    val doc = new SolrInputDocument()
    for ((k, v) <- rec) {
      doc.addField(k, v)
    }
    return doc
  }

  
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("Importer") {
      
    	arg[String]("solr-url") action { (str: String, c: Config) => c.copy(solrUrl = str)}
    	
        arg[String]("format") action {
        	(str: String, c: Config) => str match {
              case "plain" => c.copy(format = PlainTextFormat())
              case "reverb" => c.copy(format = GroupedReVerbFormat())
              case "openie4"=> c.copy(format = ClusteredOpenIE4Format())
              case "probase"=> c.copy(format = ProbaseFormat())
            } 
        } text("format (plain|reverb|openie4|probase)")
        
        opt[String]('n', "namespace") optional() action {
        	(str: String, c: Config) => c.copy(namespace = str)
        } text("default namespace")
        
        opt[String]('p', "idprefix") optional() action {
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
    for (rec <- config.format.groupIterator) {
      val doc = recordToSolr(rec)
      
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