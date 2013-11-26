package edu.knowitall.triplestore
import scopt.OptionParser
import java.util.concurrent.atomic.AtomicLong
import org.apache.solr.common.SolrInputField
import org.apache.solr.common.SolrInputDocument
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.tokenize.ClearTokenizer


object Dumper {
  
  case class Config(
    val format: String = "plain",
    val namespace: String = "",
    val idPrefix: String = "")
    
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("Dumper") {

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
  
  def field2str(f: SolrInputField) = if (f != null) 
    f.getValue() match {
    	case s: String => Some(s)
    	case _ => None
  	}
  else {
    None
  }
  
  val tokenizer = new ClearTokenizer()
  val stemmer = new MorphaStemmer()
  val tagger = new StanfordPostagger
  
  
  
  def normalize(s: String) = {
    val noTab = s.replaceAll("\t", " ")
    try {
      val toks = tokenizer.tokenize(noTab)
      val tags = tagger.postagTokenized(toks)
      val stems = tags.map(t => stemmer.lemmatizePostaggedToken(t).lemma.toLowerCase())
      stems.mkString(" ")
    } catch {
      case e: Throwable => noTab.toLowerCase()
    }
  }
  
  case class SimpleTuple(id: String, arg1: String, rel: String, arg2: String) {
    val arg1n = normalize(arg1)
    val reln = normalize(rel)
    val arg2n = normalize(arg2)
    override def toString = s"$arg1n\t$reln\t$arg2n"
  }
  
  def getField(doc: SolrInputDocument, n: String) = field2str(doc.getField(n))
  
  def doc2tuple(doc: SolrInputDocument) = for {
    arg1 <- getField(doc, "arg1")
    rel <- getField(doc, "rel")
    arg2 <- getField(doc, "arg2")
    id <- getField(doc, "id")
  } yield SimpleTuple(id, arg1, rel, arg2)
  
  def run(config: Config) = {
    val namespace = config.namespace
    val idPrefix = if (config.idPrefix.nonEmpty) config.idPrefix else namespace
    val id = new AtomicLong(0)
    def getNextId() =  idPrefix + id.getAndIncrement()
    val idFactory = getNextId _
    val format = config.format match {
      case "plain" =>   PlainTextFormat(config.namespace, idFactory)
      case "reverb" =>  GroupedReVerbFormat(config.namespace, idFactory)
      case "openie4" => ClusteredOpenIE4Format(config.namespace, idFactory)
      case "probase" => ProbaseFormat(config.namespace, idFactory)
      case _ => throw new IllegalArgumentException("Invalid format: " + config.format)
    }
    var start = System.currentTimeMillis()
    for {
      doc <- format.docIterator
      tup <- doc2tuple(doc)
    } println(tup)
    var runTime = (System.currentTimeMillis() - start) / 1000
    System.err.println()
    System.err.println("Added " + id.get + " docs in " + runTime + "secs")
  }

}