package edu.knowitall.eval.qa

import edu.knowitall.eval.OutputRecord
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.Search

case class QAOutputRecord(question: String, answer: String, ascore: Double, derivation: String) extends OutputRecord {
  override def toString = s"$question\t$answer\t$score\t$derivation"
  override def input = question
  override def output = answer
  override def score = ascore
}
case object QAOutputRecord {
  def fromLine(line: String) = line.split("\t") match {
    case Array(q, a, s, d) => QAOutputRecord(q, a, s.toDouble, d)
    case _ => throw new IllegalArgumentException(s"Could not parse line: $line")
  }
  def fromScoredAnswerGroup(question: String, group: ScoredAnswerGroup) = {
    val answer = group.answerString
    val derivs = group.derivations.map(derivationToString).mkString("; ")
    QAOutputRecord(question, answer, group.score, derivs)
  }
  def derivationToString(d: AnswerDerivation) = {
    val answer = d.answerString
    val para = d.paraphrase.target
    val query = d.parsedQuery.toString
    val equery = d.execTuple.query.toString
    val tuple = project(d.execTuple.tuple)
    s"$para => $query => $equery => $tuple => $answer"
  }
  def project(t: Tuple): String = {
    val tup = Search.ProjectTriples(List(t)).toList(0)
    tup.toString
  }
}