package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation

trait AnswerScorer {
  def scoreAnswer(answer: String, derivs: List[AnswerDerivation]): ScoredAnswer
}

trait ScoredAnswer {
  def answer: String
  def derivs: List[AnswerDerivation]
  def score: Double
}

case class BasicScoredAnswer(answer: String, derivs: List[AnswerDerivation], 
    score: Double) extends ScoredAnswer
    
case class UniformAnswerScorer(s: Double = 0.0) extends AnswerScorer {
  override def scoreAnswer(answer: String, derivs: List[AnswerDerivation]) = 
    BasicScoredAnswer(answer, derivs, s)
}
