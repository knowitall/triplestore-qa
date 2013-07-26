package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup

trait AnswerScorer {
  def scoreAnswer(group: AnswerGroup): ScoredAnswerGroup
}

trait ScoredAnswerGroup extends AnswerGroup {
  def score: Double
  def answer: List[String]
  def alternates: List[List[String]]
  def derivations: List[AnswerDerivation]
}

case class BasicScoredAnswer(answer: List[String], alternates: List[List[String]], 
    derivations: List[AnswerDerivation], score: Double) 
    extends ScoredAnswerGroup
    
case class UniformAnswerScorer(s: Double = 0.0) extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup) = 
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, s)
}



case class NumDerivationsScorer() extends AnswerScorer {
  override def scoreAnswer(group: AnswerGroup) = 
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, 
        group.derivations.size.toDouble)
}