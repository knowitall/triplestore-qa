package edu.knowitall.search

import edu.knowitall.execution.ExecTuple

case class AnswerState(answer: String, execTuple: ExecTuple) extends QAState