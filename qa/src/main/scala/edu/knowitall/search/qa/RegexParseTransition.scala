package edu.knowitall.search.qa

import edu.knowitall.search.Transition
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.parsing.regex.RegexQuestionPattern
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.postag.Postagger

class RegexParseTransition(parser: RegexQuestionParser) extends Transition[QaState, QaAction] {
  
  override def apply(s: QaState) = s match {
    case s: QuestionState => parseQuestion(s)
    case _ => Nil
  }
  
  def parseQuestion(s: QuestionState) = {
    val question = s.question
    for {
      (pattern, query) <- parser.parseWithPattern(question)
      newState = QueryState(query)
    } yield (pattern, newState)
    
  }

}

object RegexParseTransition {
  lazy val defaultParser = new RegexQuestionParser 
}