package edu.knowitall.search

class QASearchProblem(question: String) extends SearchProblem[QAState, QAAction] {
  
  type Path = SearchPath[QAState, QAAction]
  
  val initialState = QuestionState(question)
  
  override def successors(s: QAState) = s match {
    case qs: QuestionState => questionSuccessors(qs)
    case qs: QueryState => querySuccessors(qs)
    case as: AnswerState => answerSuccessors(as)
    case _ => Nil
  }
  
  def questionSuccessors(s: QuestionState) = null
  
  def querySuccessors(s: QueryState) = null
  
  def answerSuccessors(s: AnswerState) = null
  
  override def isGoal(s: QAState) = s match {
    case as: AnswerState => true
    case _ => false
  }
  
  override def cost(p:Path) = 0.0 

}