package edu.knowitall.qa

import org.scalatest._
import org.apache.solr.client.solrj.util.ClientUtils

class LexItemConverterTest extends FlatSpec {
  
  
  def words(str: String) = str.split(" ") map QWord.qWordWrap
  
  def tokens(str: String) = str.split(" ") map QToken.qTokenWrap
  
  "LexItemConverter" should "convert to a solr doc and back correctly" in {
   
    val item1 = EntItem(words("paris"), "paris.e")
    val item2 = RelItem(words("the capital"), "capital.r", ArgOrder.fromInt(0))
    val item3 = QuestionItem(tokens("What is $r of $y ?"), ArgOrder.fromInt(1))
    
    def identity(item: LexItem) = {
      val inputDoc = LexItemConverter.itemToDoc(item)
      val doc = ClientUtils.toSolrDocument(inputDoc)
      LexItemConverter.docToItem(doc)
    }
    
    for (lexItem <- Seq(item1, item2, item3)) {
      assert(identity(lexItem) === lexItem)
    }
  }
}