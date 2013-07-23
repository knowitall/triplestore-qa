package edu.knowitall.execution

trait AnswerGrouper {
  def group(derivs: List[AnswerDerivation]): List[AnswerGroup]
}

trait AnswerGroup {
  def answer: List[String]
  def alternates: List[List[String]]
  def derivations: List[AnswerDerivation]
}

case class BasicAnswerGroup(answer: List[String], alternates: List[List[String]], 
    derivations: List[AnswerDerivation]) extends AnswerGroup
    
case class BasicAnswerGrouper(
    norm: String => String = BasicAnswerGrouper.normalize)
    extends AnswerGrouper {
  
  override def group(derivs: List[AnswerDerivation]): List[AnswerGroup] = {
    val grouped = derivs.groupBy(d => d.answer.map(norm))
    val groups = for ((answers, grp) <- grouped;
                      alts = grp.map(_.answer).distinct) 
                      yield BasicAnswerGroup(answers, alts, grp)
    groups.toList
  }
  
}
case object BasicAnswerGrouper {
  def normalize(s: String): String = StrSim.norm(s)
}