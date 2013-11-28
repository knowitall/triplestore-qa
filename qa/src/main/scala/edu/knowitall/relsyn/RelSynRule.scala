package edu.knowitall.relsyn

import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.search.qa.QaAction

case class RelSynRule(rel1: String, rel2: String, inverted: Boolean, 
    count1: Double, count2: Double, jointCount: Double, pmi: Double) 
    extends QaAction {
  
  private def swapArgs(c: TConjunct) = for {
    a1 <- c.values.get(Search.arg1)
    a2 <- c.values.get(Search.arg2)
    newvals = c.values ++ List((Search.arg1 -> a2), (Search.arg2 -> a1))
  } yield c.copy(values = newvals)
  
  private def adjustSwap(c: TConjunct) = if (inverted) swapArgs(c) else Some(c)
  
  private def replaceRel(c: TConjunct) = {
    val newr = QuotedTLiteral(rel2)
    Some(c.copy(values = c.values + (Search.rel -> newr)))
  }
  
  private def relValue(c: TConjunct) = for {
    value <- c.values.get(Search.rel)
    svalue <- value match {
      case l: TLiteral => Some(l.value)
      case _ => None
    }
  } yield svalue
  
  def apply(c: TConjunct) = for {
    svalue <- relValue(c)
    withNewRel <- replaceRel(c)
    newc <- adjustSwap(withNewRel)
  } yield newc
  
}