package edu.knowitall.scoring.learning

trait HiddenVariableModel[Input, Hidden, Output] {
  type Model = HiddenVariableModel[Input, Hidden, Output]
  def predict(input: Input): Option[Hidden]
  def predictConstrained(input: Input, output: Output): Option[Hidden]
  def project(hidden: Hidden): Output
  def score(input: Input, hidden: Hidden): Double
  def update(input: Input, predicted: Hidden, expected: Hidden): Model
  def isCorrect(input: Input, predicted: Output, expected: Output): Boolean
  def weights: SparseVector
  def sum(that: Model): Model
  def scale(c: Double): Model
}