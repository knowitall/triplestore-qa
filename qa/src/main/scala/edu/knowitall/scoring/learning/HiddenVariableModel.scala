package edu.knowitall.scoring.learning

trait HiddenVariableModel[Input, Output] {
  type Model = HiddenVariableModel[Input, Output]
  def predict(input: Input): Option[Output]
  def candidatePredictions(input: Input): Seq[Output]
  def update(input: Input, predicted: Output, expected: Output): Unit
  def weights: SparseVector
  def sum(that: Model): Unit
  def scale(c: Double): Unit
}