package edu.knowitall.paraphrasing.template

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.lm.KenLmServer


class TemplateParaphraseScorer() {
  val client = new KenLmServer()
  def scoreAll(derivs: Iterable[TemplateParaphraseDerivation]) = {
    val dlist = derivs.toList
    val lmScores = client.query(dlist.map(_.paraphrase.question.mkString(" "))).map(_._2)
    for ((d, score) <- dlist.zip(lmScores)) yield d.copy(score = -score * d.templates.pmi) 
  }
}