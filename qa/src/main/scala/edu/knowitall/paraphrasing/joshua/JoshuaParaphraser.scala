package edu.knowitall.paraphrasing.joshua

import edu.knowitall.paraphrasing.Paraphraser
import edu.knowitall.paraphrasing.Paraphrase
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.tokenize.ClearTokenizer
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer

case class JoshuaParaphraser(client: JoshuaClient) extends Paraphraser {
  
  lazy val tagger = new StanfordPostagger()
  lazy val tokenizer = new ClearTokenizer()
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def this() = this(new JoshuaClient())
  
  def stemString(s: String): String = {
    val tokens = tokenizer(s)
    val tagged = tagger.postagTokenized(tokens)
    val lemmas = tagged.map(t => MorphaStemmer.lemmatizePostaggedToken(t).lemma.toLowerCase()) 
    lemmas.mkString(" ")
  }
  
  override def paraphrase(s: String): List[Paraphrase] = {
    val stemmed = stemString(s)
    val derivs = client.decode(stemmed)
    val paras = derivs.map(d => Paraphrase(s, d.output, d))
    paras.sortBy(p => -p.derivation.score)
  }

}