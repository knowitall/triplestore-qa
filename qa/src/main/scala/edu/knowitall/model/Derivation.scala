package edu.knowitall.model

import edu.knowitall.search.qa.AnswerState
import edu.knowitall.search.qa.QuestionState
import edu.knowitall.search.Edge
import edu.knowitall.search.qa.QaState
import edu.knowitall.search.qa.QaAction
import edu.knowitall.learning.SparseVector
import edu.knowitall.search.qa.QaStep

case class Derivation(question: String,
					  answer: String,
					  steps: IndexedSeq[QaStep],
					  features: SparseVector,
					  score: Double)