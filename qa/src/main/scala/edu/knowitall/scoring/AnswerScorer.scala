package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGroup

/**
 * An AnswerRanker takes a set of AnswerGroups and sorts them in some useful way.
 */
trait AnswerRanker {
  def rankAnswers(originalQuestion: String, groups: Seq[AnswerGroup]): Seq[ScoredAnswerGroup]
}

/**
 * An AnswerScorer is an AnswerRanker whose ordering is defined by an explicit score.
 */
trait AnswerScorer extends AnswerRanker {

  override def rankAnswers(originalQuestion: String, groups: Seq[AnswerGroup]): Seq[ScoredAnswerGroup] = {
    groups.map(group => scoreAnswer(originalQuestion, group)).sortBy(-_.score)
  }

  def scoreAnswer(originalQuestion: String, group: AnswerGroup): ScoredAnswerGroup
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
  override def scoreAnswer(originalQuestion: String, group: AnswerGroup) =
    BasicScoredAnswer(group.answer, group.alternates, group.derivations, s)
}

case class NumDerivationsScorer() extends AnswerScorer {
  override def scoreAnswer(originalQuestion: String, group: AnswerGroup) =
    BasicScoredAnswer(group.answer, group.alternates, group.derivations,
        group.derivations.size.toDouble)
}