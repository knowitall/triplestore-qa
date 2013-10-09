package edu.knowitall.paralex

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval

case class ArgQuestion(question: Seq[String], argInterval: Interval)

case class ParaphraseDerivation(question: ArgQuestion, 
    paraphrase: ArgQuestion, templates: TemplatePair)
    
trait ParaphraseGenerator {
  def generate(question: Seq[String]): Iterable[ParaphraseDerivation]
}

class SolrParaphraseGenerator(url: String, maxHits: Int = 500, maxArgLength: Int = 4) extends ParaphraseGenerator {
  val client = new ParaphraseTemplateClient(url)
  def intervals(size: Int) =
    for (i <- Range(0, size); j <- Range(i, size); if j+1-i <= maxArgLength) yield Interval.open(i, j+1)
  
  def templates(q: ArgQuestion): List[TemplatePair] = {
    val i = q.argInterval.start
    val j = q.argInterval.end
    val n = q.question.size
    val left = q.question.slice(0, i).mkString(" ")
    val right = q.question.slice(j, n).mkString(" ")
    val query = left + " $y " + right
    client.paraphrases(query, maxHits)
  }
  
  def abstractQuestion(q: Seq[String]): Iterable[ArgQuestion] = {
    val n = q.size
    for (i <- intervals(n)) yield ArgQuestion(q, i)
  }
  
  def substitute(q: ArgQuestion, t: TemplatePair): ArgQuestion = {
    val templ = t.template2
    val templSeq = templ.split(" ").toSeq
    val arg = q.question.slice(q.argInterval.start, q.argInterval.end)
    val i = templSeq.indexOf("$y")
    if (i >= 0) {
      val left = templSeq.slice(0, i)
      val right = templSeq.slice(i+arg.size, templSeq.size)
      val para = ArgQuestion(left ++ arg ++ right, Interval.open(i, i+arg.size))
      para
    } else {
      throw new IllegalArgumentException(s"Could not find var in: $templ")
    }
    
  }
  
  override def generate(question: Seq[String]): Iterable[ParaphraseDerivation] = {
    for (aq <- abstractQuestion(question); t <- templates(aq); para = substitute(aq, t)) 
      yield ParaphraseDerivation(aq, para, t)
      
  }

}