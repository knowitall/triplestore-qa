package edu.knowitall.derivation

import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ExecTuple

trait PartialDerivation

trait ParaphraseDerivation extends PartialDerivation {
  def question: String
  def paraphrase: String
}

trait ParseDerivation extends PartialDerivation {
  def paraphraseDerivation: ParaphraseDerivation
  def query: ConjunctiveQuery
}

trait ExecutionDerivation extends PartialDerivation {
  def parseDerivation: ParseDerivation
  def executedQuery: ConjunctiveQuery
  def result: ExecTuple
}

trait AnswerDerivation extends PartialDerivation {
  def answer: String
  def executionDerivation: ExecutionDerivation
}