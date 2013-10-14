package edu.knowitall.paraphrasing

trait ParaphraseDerivation

trait ScoredParaphraseDerivation extends ParaphraseDerivation {
  def score: Double
}

object IdentityDerivation extends ScoredParaphraseDerivation {
  override val score = 1.0
}