package edu.knowitall.execution

trait AnswerGrouper {
  def group(derivs: List[AnswerDerivation]): List[AnswerGroup]
}

trait AnswerGroup {
  def answer: String
  def alternates: List[String]
  def derivations: List[AnswerDerivation]
}

case class BasicAnswerGroup(answer: String, alternates: List[String], 
    derivations: List[AnswerDerivation]) extends AnswerGroup
    
case class BasicAnswerGrouper(
    norm: String => String = BasicAnswerGrouper.normalize)
    extends AnswerGrouper {
  
  override def group(derivs: List[AnswerDerivation]): List[AnswerGroup] = {
    val grouped = derivs.groupBy(d => norm(d.answer))
    val groups = for ((answer, grp) <- grouped;
                      alts = grp.map(_.answer).distinct) 
                      yield BasicAnswerGroup(answer, alts, grp)
    groups.toList
  }
  
}
case object BasicAnswerGrouper {
  def normalize(s: String): String = StrSim.norm(s)
}