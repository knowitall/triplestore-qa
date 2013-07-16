package edu.knowitall.parsing

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.util.ClientUtils
import scala.collection.JavaConverters._

class SolrLexicon(val server: SolrServer) extends Lexicon {

  def this(url: String = "http://rv-n13.cs.washington.edu:8888/solr") = this(new HttpSolrServer(url))
  
  private def escape(s: String): String = ClientUtils.escapeQueryChars(s)
  
  private def queryString(words: IndexedSeq[QToken]) = {
    val wordStrings = words.map(_.toString) 
    "+tokens_exact:\"%s\"".format(wordStrings.map(escape).mkString(" "))
  }

  private def buildQuery(words: IndexedSeq[QToken]): SolrQuery = {
    new SolrQuery(queryString(words))
      .addSort("weight", SolrQuery.ORDER.desc)
  }
  
  private def buildCountQuery(words: IndexedSeq[QToken]): SolrQuery = {
    new SolrQuery(queryString(words)).setRows(0)
  }
  
  private def execQuery(sq: SolrQuery): Iterable[LexItem] = {
    val resp = server.query(sq)
    resp.getResults().asScala.map(LexItemConverter.docToItem)
  }
  
  def get(words: IndexedSeq[QToken]): Iterable[LexItem] = {
    
    execQuery(buildQuery(words))
  }
  
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem] = 
    get(words) filter (_.isInstanceOf[RelItem]) map (_.asInstanceOf[RelItem])
    
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem] = 
    get(words) filter (_.isInstanceOf[EntItem]) map (_.asInstanceOf[EntItem])
    
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem] = 
    get(words) filter (_.isInstanceOf[QuestionItem]) map (_.asInstanceOf[QuestionItem])
  
  def getQuestionRel(words: IndexedSeq[QToken]): Iterable[QuestionRelItem] = {
    throw new RuntimeException("Method not implemented.")
  }
    
  def has(words: IndexedSeq[QToken]): Boolean = {
    server.query(buildCountQuery(words)).getResults().getNumFound() > 0
  }
}

object SolrLexiconTest extends App {

   val solrLexicon = new SolrLexicon()
   val referenceLexItems = new EvalLexiconLoader(args(0))
   
   var numWrong = 0
   var numRight = 0
   
   def total = numWrong + numRight
   def pct = numWrong.toDouble / total.toDouble
   def bleat = println("Total: %d, right: %d, wrong: %d, %%Wrong: %.02f"
       .format(total, numRight, numWrong, pct))

   referenceLexItems.zipWithIndex foreach { case (item, index) =>
     if (!solrLexicon.get(item.words).map(_.asInstanceOf[LexItem]).toSet.contains(item)) {
       numWrong += 1
       println(s"Solr doesn't seem to have an entry (#$index) for: $item")
     } else {
       numRight += 1
     }
     if (index % 10000 == 0) bleat
   }

   bleat
}