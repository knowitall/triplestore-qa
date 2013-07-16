package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup

trait AnswerScorer {
  def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup
}

trait ScoredAnswerGroup {
  def group: AnswerGroup
  def score: Double
}

case class BasicScoredAnswer(group: AnswerGroup, score: Double) 
  extends ScoredAnswerGroup
    
case class UniformAnswerScorer(s: Double = 0.0) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup) = BasicScoredAnswer(group, s)
}
