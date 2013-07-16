package edu.knowitall.qa

import org.scalatest._
import org.apache.solr.client.solrj.util.ClientUtils

class LexItemConverterTest extends FlatSpec {
  
  
  def words(str: String) = str.split(" ") map QWord.qWordWrap
  
  def tokens(str: String) = str.split(" ") map QToken.qTokenWrap
  
  "LexItemConverter" should "convert to a solr doc and back correctly" in {
   
    val item1 = new EntItem(words("paris"), "paris.e") with Weight { val weight = 0.0 } 
    val item2 = new RelItem(words("the capital"), "capital.r", ArgOrder.fromInt(0)) with Weight { val weight = 1.0 }
    val item3 = new QuestionItem(tokens("What is $r of $y ?"), ArgOrder.fromInt(1)) with Weight { val weight = 2.0 }
    
    def identity(item: LexItem with Weight) = {
      val inputDoc = LexItemConverter.itemToDoc(item)
      val doc = ClientUtils.toSolrDocument(inputDoc)
      doc.addField("_version_", 0)
      LexItemConverter.docToItem(doc)
    }
    
    for (lexItem <- Seq(item1, item2, item3)) {
      assert(identity(lexItem) === lexItem)
    }
  }
}