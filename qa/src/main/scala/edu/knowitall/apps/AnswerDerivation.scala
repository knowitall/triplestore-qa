package edu.knowitall.apps

import edu.knowitall.paraphrasing.Paraphrase
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.ConjunctiveQuery

case class AnswerDerivation(question: String,
							paraphrase: Paraphrase, 
							parsedQuery: ConjunctiveQuery,
							execTuple: ExecTuple) {
  def answer: List[String] = execTuple.answer
  def answerString = execTuple.answerString
}