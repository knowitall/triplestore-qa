package edu.knowitall.qa

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.common.{SolrDocument, SolrInputDocument}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

class SolrLexiconBuilder(server: SolrServer, lexItems: Iterable[LexItem]) {

  import LexItemConverter._

  def this(url: String, lexItems: Iterable[LexItem]) = this(new ConcurrentUpdateSolrServer(url, 1000, 4), lexItems)
  
  def go: Unit = {
    
    lexItems.map(itemToDoc).zipWithIndex.foreach { case (doc, num) =>
      if (num % 1000 == 0) System.out.println(s"$num docs indexed.")
      server.add(doc)
    }
    
    server.commit()
  }  
  
  def close(): Unit = server.shutdown()
}

object SolrLexiconBuilder extends App {
  
  import java.io.File
  import scopt.OptionParser
  import edu.knowitall.common.Resource.using
  
  case class Config(val url: String = ".", val dataPath: File = new File("."))

  val parser = new OptionParser[Config]("SolrLexiconBuilder") {
    arg[String]("solrUrl") action { (x, c) => c.copy(url = x) } text ("Solr URL")
    arg[File]("dataPath") action { (x, c) => c.copy(dataPath = x) } text ("Path to Paralex Evaluation Data")
  }

  parser.parse(args, Config()) foreach { config =>
    val dbVocab = new File(config.dataPath, "database/vocab.txt")
    val lexVocab = new File(config.dataPath, "lexicons/paralex/vocab.txt")
    val lexItems = new File(config.dataPath, "lexicons/paralex/lexicon.txt")
    
    val items = new EvalLexiconLoader(dbVocab, lexVocab, lexItems)
    
    using(new SolrLexiconBuilder(config.url, items)) { builder => builder.go }
  }
}


object LexItemConverter {
  
  val idCounter = new AtomicInteger(0)
  
  private def encode(o: ArgOrder) = ArgOrder.toInt(o).toString
  
  def itemToDoc(item: LexItem): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("id", idCounter.getAndIncrement().toString)
    val tokenString = item.words.mkString(" ")
    doc.addField("tokens", tokenString)
    doc.addField("tokens_exact", tokenString)
    item match {
      case EntItem(tokens, entity) => {
        doc.addField("entity", entity)
      }
      case RelItem(tokens, relation, argOrder) => {
        doc.addField("relation", relation)
        doc.addField("argOrder", encode(argOrder))
      }
      case QuestionItem(token, argOrder: ArgOrder) => {
        doc.addField("argOrder", encode(argOrder))
      }
    }
    doc
  }
  
  private val commonFields = Set("id", "tokens", "tokens_exact")
  private val entItemFields = commonFields ++ Set("entity")
  private val relItemFields = commonFields ++ Set("relation", "argOrder")
  private val questionItemFields = commonFields ++ Set("argOrder")
  
  private def getFieldNames(doc: SolrDocument): Set[String] = doc.getFieldNames.asScala.toSet
  private def getFields(doc: SolrDocument): Map[String, String] = {
    getFieldNames(doc).map(name => (name, doc.getFieldValue(name).asInstanceOf[String] )).toMap
  }
  
  private def docToEntItem(doc: SolrDocument): EntItem = {
    require(getFieldNames(doc) == entItemFields, "Incorrect fields for EntItem: " + getFieldNames(doc))
    val fields = getFields(doc)
    val tokens = fields("tokens_exact").asInstanceOf[String].split(" ").map(QWord.qWordWrap)
    EntItem(tokens, fields("entity"))
  }
  
  private def docToRelItem(doc: SolrDocument): RelItem = {
    require(getFieldNames(doc) == relItemFields, "Incorrect fields for RelItem: " + getFieldNames(doc))
    val fields = getFields(doc)
    val tokens = fields("tokens_exact").asInstanceOf[String].split(" ").map(QWord.qWordWrap)
    RelItem(tokens, fields("relation"), ArgOrder.fromInt(fields("argOrder").toInt))
  }
  
  private def docToQuestionItem(doc: SolrDocument): QuestionItem = {
    require(getFieldNames(doc) == questionItemFields, "Incorrect fields for QuestionItem: " + getFieldNames(doc))
    val fields = getFields(doc)
    val tokens = fields("tokens_exact").asInstanceOf[String].split(" ").map(QToken.qTokenWrap)
    QuestionItem(tokens, ArgOrder.fromInt(fields("argOrder").toInt))
  }
  
  def docToItem(doc: SolrDocument): LexItem = {
    val fieldNames = getFieldNames(doc)
    if (fieldNames == entItemFields) docToEntItem(doc)
    else if (fieldNames == relItemFields) docToRelItem(doc)
    else if (fieldNames == questionItemFields) docToQuestionItem(doc)
    else throw new RuntimeException("Unknown field names: " + fieldNames)
  }
}