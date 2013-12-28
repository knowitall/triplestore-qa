package edu.knowitall.parsing.cg

import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.typer.Type

trait Combinator {
  def apply(left: Category, right: Category): Option[Category]
}

trait TerminalRule {
  def apply(interval: Interval, sent: Sentence with Chunked with Lemmatized): Option[Category]
}

object RightApply extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (b: Binary, a: Arg) => {
      val newQuery = b.query.subs(b.rightVar, a.value)
      Some(Unary(b.leftVar, newQuery))
    }
    case _ => None 
  }
  override def toString = "RightApply"
}

object LeftApply extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (a: Arg, b: Binary) => {
      val newQuery = b.query.subs(b.leftVar, a.value)
      Some(Unary(b.rightVar, newQuery))
    }
    case _ => None 
  }
  override def toString = "LeftApply"
}

object UnaryIntersect extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (u1: Unary, u2: Unary) => {
      val newVar = TVariable(u1.freeVar.name + u2.freeVar.name + "x")
      val oldVar1 = u1.freeVar
      val oldVar2 = u2.freeVar
      val query1 = u1.query.subs(oldVar1, newVar)
      val query2 = u2.query.subs(oldVar2, newVar)
      val newQuery = query1.combine(query2).subs(newVar, TVariable("x"))
      Some(Unary(newVar, newQuery))
    }
    case _ => None
  }
  override def toString = "UnaryIntersect"
}

object UnaryIdentity extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (Identity, u: Unary) => Some(u)
    case (u: Unary, Identity) => Some(u)
    case _ => None
  }
  override def toString = "UnaryIdentity"
}
