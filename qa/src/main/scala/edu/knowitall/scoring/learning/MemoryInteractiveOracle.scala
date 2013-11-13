package edu.knowitall.scoring.learning

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.eval.FileOracle
import org.slf4j.LoggerFactory

class MemoryInteractiveOracle(oracle: FileOracle) extends CorrectnessModel[String, AnswerDerivation] {
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def this(path: String) = this(new FileOracle(path))
  
  val labeled = new LabeledDataOracle(oracle)
  val interactive = new InteractiveOracle()
  
  override def isCorrect(question: String, deriv: AnswerDerivation) = {
    val answer = deriv.answerString
    if (oracle.hasLabel(question, answer)) {
      logger.debug(s"Using saved labels for $question")
      labeled.isCorrect(question, deriv)
    } else {
      val result = interactive.isCorrect(question, deriv)
      oracle.update(question, answer, result)
      oracle.save
      result
    }
  }
  
  private def haveLabelFor(question: String, deriv: AnswerDerivation) = 
    oracle.hasLabel(question, deriv.answerString)
  
  override def pickCorrect(question: String, derivs: Seq[AnswerDerivation]) = {
    labeled.pickCorrect(question, derivs) match {
      case Some(deriv) => {
        logger.debug(s"Using saved labels for $question")
        Some(deriv)
      }
      case None => {
        val unlabeled = derivs.filter(d => !haveLabelFor(question, d))
        interactive.pickCorrectMultiple(question, unlabeled) match {
          case seq: Seq[AnswerDerivation] if seq.size > 0 => {
            for (d <- seq) {
              oracle.update(question, d.answerString, true)
            }
            oracle.save
            Some(seq(0))
          }
          case Nil => {
            for (d <- unlabeled) {
              oracle.update(question, d.answerString, false)
            }
            oracle.save
            None
          }
        }
        
      }
    }
  }

}