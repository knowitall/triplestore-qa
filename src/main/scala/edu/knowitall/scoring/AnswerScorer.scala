package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup

trait AnswerScorer {
  def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup
}

trait ScoredAnswerGroup extends AnswerGroup {
  def group: AnswerGroup
  def score: Double
  val answer = group.answer
  val alternates = group.alternates
  val derivations = group.derivations
}

case class BasicScoredAnswer(group: AnswerGroup, score: Double) 
  extends ScoredAnswerGroup
    
case class UniformAnswerScorer(s: Double = 0.0) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup) = BasicScoredAnswer(group, s)
}
