package edu.knowitall.apps

trait AnswerGenerator {
  
  def generateAnswers(question: String): Iterable[AnswerDerivation]
  def apply(question: String) = generateAnswers(question)

}