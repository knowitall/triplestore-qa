package edu.knowitall.scoring.learning

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.eval.Oracle
import edu.knowitall.eval.FileOracle

class LabeledDataOracle(oracle: Oracle) extends CorrectnessModel[String, AnswerDerivation] {
  
  def this(path: String) = this(new FileOracle(path))
  
  def isCorrectAnswer(question: String, answer: String) =
    oracle.getLabel(question, answer).getOrElse(false)
    
  override def isCorrect(question: String, deriv: AnswerDerivation) =
    isCorrectAnswer(question, deriv.answerString)
  
  override def pickCorrect(question: String, derivs: Seq[AnswerDerivation]) =
    derivs.filter(answer => isCorrect(question, answer)) match {
      case d :: ds => Some(d)
      case Nil => None
    }

}