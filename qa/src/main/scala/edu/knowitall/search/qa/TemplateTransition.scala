package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.paraphrasing.template.ParaphraseTemplateClient
import com.typesafe.config.ConfigFactory
import edu.knowitall.paraphrasing.template.TemplatePair

class TemplateTransition(client: ParaphraseTemplateClient) extends Transition[QaState, QaAction] {
  
  def this() = this(new ParaphraseTemplateClient())
  
  override def apply(s: QaState) = s match {
    case s: AbstractedArgState => paraphrase(s)
    case _ => Nil
  }
  
  private def paraphrase(state: AbstractedArgState) = 
    for {
      templatePair <- client.paraphrases(state.queryString)
      arg = state.arg
      newQuestion = applyTemplate(arg, templatePair)
      action = templatePair
      newState = QuestionState(newQuestion, true)
    } yield (action, newState)
  
  private def applyTemplate(arg: String, pair: TemplatePair) = 
    pair.template2.replace("$y", arg)

}
