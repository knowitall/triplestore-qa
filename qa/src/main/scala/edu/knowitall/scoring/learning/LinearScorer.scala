package edu.knowitall.scoring.learning

case class LinearScorer[T](features: Function[T, SparseVector], weights: SparseVector) {
  def apply(t: T): Double = features(t) * weights 
}