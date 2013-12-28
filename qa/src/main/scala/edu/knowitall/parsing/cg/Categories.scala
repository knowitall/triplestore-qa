package edu.knowitall.parsing.cg

import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.ConjunctiveQuery

trait Category

case class Arg(value: TLiteral) extends Category

case class Unary(freeVar: TVariable, query: ConjunctiveQuery) extends Category {
  def intersect(that: Unary): Unary = {
    val newVar = TVariable(this.freeVar.name + that.freeVar.name)
    val oldVar1 = this.freeVar
    val oldVar2 = that.freeVar
    val query1 = this.query.subs(oldVar1, newVar)
    val query2 = that.query.subs(oldVar2, newVar)
    val newQuery = query1.combine(query2).subs(newVar, Unary.finalVar)
    Unary(newVar, newQuery)
  }
}
case object Unary {
  val finalVar = TVariable("x")
}

case class Binary(leftVar: TVariable, rightVar: TVariable, 
    query: ConjunctiveQuery) extends Category

case class RelMod(value: String) extends Category
    
object Identity extends Category {
  override def toString = "Identity"
}