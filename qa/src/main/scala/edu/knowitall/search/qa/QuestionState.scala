package edu.knowitall.search.qa

trait QuestionState extends QaState {
  def question: String
  def isParaphrased: Boolean
}

case object QuestionState {
  private case class QuestionStateImpl(question: String, 
      isParaphrased: Boolean = false) extends QuestionState
  def apply(question: String): QuestionState = QuestionStateImpl(question)
  def apply(q: String, isP: Boolean): QuestionState = QuestionStateImpl(q, isP)
}