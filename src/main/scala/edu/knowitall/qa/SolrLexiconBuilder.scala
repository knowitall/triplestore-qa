package edu.knowitall.qa

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.common.{SolrDocument, SolrInputDocument}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.JavaConversions._

class SolrLexiconBuilder(server: SolrServer, lexItems: Iterable[LexItem with Weight]) {

  import LexItemConverter._

  def this(url: String, lexItems: Iterable[LexItem with Weight]) = this(new ConcurrentUpdateSolrServer(url, 1000, 4), lexItems)
  
  def go: Unit = {
    
    lexItems.iterator.map(itemToDoc).zipWithIndex.foreach { case (doc, num) =>
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
  
  val idCounter = new AtomicLong(0)
  
  private def encode(o: ArgOrder) = ArgOrder.toInt(o)
  
  def itemToDoc(item: LexItem with Weight): SolrInputDocument = {
    val doc = new SolrInputDocument
    doc.addField("id", idCounter.getAndIncrement())
    doc.addField("weight", item.weight)
    val tokenString = item.words.mkString(" ")
    doc.addField("tokens", tokenString)
    doc.addField("tokens_exact", tokenString)
    item.asInstanceOf[LexItem] match {
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
  
  private val commonFields = Set("id", "tokens", "tokens_exact", "weight")
  private val entItemFields = commonFields ++ Set("entity")
  private val relItemFields = commonFields ++ Set("relation", "argOrder")
  private val questionItemFields = commonFields ++ Set("argOrder")
  
  private def getFieldNames(doc: SolrDocument): Set[String] = doc.getFieldNames.toSet
  private def getFieldMap(doc: SolrDocument): Map[String, Any] = {
    doc.getFieldNames.map { fieldName =>
      (fieldName, doc.getFieldValue(fieldName))
    } toMap
  }
  private def words(fieldMap: Map[String, Any]): IndexedSeq[QWord] = {
    fieldMap("tokens").asInstanceOf[String].split(" ") map QWord.qWordWrap
  }
  private def argOrder(fieldMap: Map[String, Any]): ArgOrder = {
    require(fieldMap.contains("argOrder"), "Undefined argOrder field.")
    val value = fieldMap.getOrElse("argOrder", throw new RuntimeException())
    ArgOrder.fromInt(fieldMap("argOrder").asInstanceOf[Int])
  }
  private def getWeight(fieldMap: Map[String, Any]): Double = fieldMap("weight").asInstanceOf[Double]
    
  private def docToEntItem(fieldMap: Map[String, Any]): EntItem with Weight = {
    new EntItem(words(fieldMap), fieldMap("entity").asInstanceOf[String]) 
    with Weight { val weight = getWeight(fieldMap) }
  }
  
  private def docToRelItem(fieldMap: Map[String, Any]): RelItem with Weight = {
    new RelItem(words(fieldMap), fieldMap("relation").asInstanceOf[String], argOrder(fieldMap))
    with Weight { val weight = getWeight(fieldMap) }
  }
  
  private def docToQuestionItem(fieldMap: Map[String, Any]): QuestionItem with Weight = {
    val tokens = fieldMap("tokens_exact").asInstanceOf[String].split(" ").map(QToken.qTokenWrap)
    new QuestionItem(tokens, argOrder(fieldMap))
    with Weight { val weight = getWeight(fieldMap) }
  }
  
  def docToItem(doc: SolrDocument): LexItem = {
    val fieldNames = getFieldNames(doc)
    val fieldMap = getFieldMap(doc)
    if (fieldNames == entItemFields) docToEntItem(fieldMap)
    else if (fieldNames == relItemFields) docToRelItem(fieldMap)
    else if (fieldNames == questionItemFields) docToQuestionItem(fieldMap)
    else throw new RuntimeException("Unrecognized LexItem field set: " + fieldNames)
  }
}