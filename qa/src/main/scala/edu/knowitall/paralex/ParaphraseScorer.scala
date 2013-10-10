package edu.knowitall.paralex

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.postag.StanfordPostagger

case class ScoredParaphraseDerivation(deriv: ParaphraseDerivation, score: Double)

trait ParaphraseScorer {
  def scoreAll(derivs: Iterable[ParaphraseDerivation]): Iterable[ScoredParaphraseDerivation]
  def score(deriv: ParaphraseDerivation): ScoredParaphraseDerivation = 
    scoreAll(List(deriv)).toList match {
    case x :: Nil => x
    case _ => ScoredParaphraseDerivation(deriv, 0)
  }
}

class PmiScorer extends ParaphraseScorer {
  override def scoreAll(derivs: Iterable[ParaphraseDerivation]) = derivs.map(d => ScoredParaphraseDerivation(d, d.templates.pmi))
}

class PmiLmScorer(url: String = "http://rv", port: Int = 8089) extends ParaphraseScorer {
  val client = KenLmServer(url, port)
  override def scoreAll(derivs: Iterable[ParaphraseDerivation]) = {
    val dlist = derivs.toList
    val scores = client.query(dlist.map(_.paraphrase.question.mkString(" "))).map(_._2)
    for ((d, score) <- dlist.zip(scores)) yield ScoredParaphraseDerivation(d, -score * d.templates.pmi)
  }
}

case class DemoResult(paraphrase: String, score: Double, arg: String, template: String, pmi: Double, lm: Double, joint: Double, marg1: Double, marg2: Double)

class ParaphraseDemo {
  val tokenizer = new ClearTokenizer()
  val tagger = new StanfordPostagger()
  val lm = new KenLmServer("http://rv", 8089)
  
  def paraphrase(q: String) = {
    val qs = tagger.postagTokens(tokenizer(q)).map(t => MorphaStemmer.lemmatizePostaggedToken(t).lemma.toLowerCase())
    val scorer = new PmiLmScorer()
    val generator = new SolrParaphraseGenerator(maxHits = 1000)
    val derivs = generator.generate(qs)
    val lms = lm.query(derivs.map(d => d.paraphrase.question.mkString(" "))).map(_._2)
    val pairs = (derivs zip lms)
    val result = for ((d, lm) <- pairs) yield DemoResult(d.paraphrase.question.mkString(" "), lm*d.templates.pmi, d.paraphrase.arg, d.templates.template2, d.templates.pmi, lm, d.templates.jointCount, d.templates.count1, d.templates.count2)
    result.toList
  }
}

object ScoreTest extends App {
  val tokenizer = new ClearTokenizer()
  val tagger = new StanfordPostagger()
  
  val q = args(0)
  val qs = tagger.postagTokens(tokenizer(q)).map(t => MorphaStemmer.lemmatizePostaggedToken(t).lemma.toLowerCase())
  println(qs)
  val scorer = new PmiLmScorer()
  val generator = new SolrParaphraseGenerator(maxHits = 1000)
  val derivs = generator.generate(qs)
  val scored = scorer.scoreAll(derivs).toList.sortBy(_.score)
  scored foreach { d => {
    println(s"${d.deriv.paraphrase.arg}\t${d.deriv.templates.template2}\t${d.deriv.paraphrase.question.mkString(" ")}")
  }}
}