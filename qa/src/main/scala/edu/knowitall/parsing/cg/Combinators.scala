package edu.knowitall.parsing.cg

import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.collection.immutable.Interval

trait Combinator {
  def apply(left: Category, right: Category): Option[Category]
}

trait TerminalRule[T] {
  def apply(interval: Interval, t: T): Option[Category]
}

object RightApply extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (b: Binary, a: Arg) => {
      val newQuery = b.query.subs(b.rightVar, a.value)
      Some(Unary(b.leftVar, newQuery))
    }
    case _ => None 
  }
}

object LeftApply extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (a: Arg, b: Binary) => {
      val newQuery = b.query.subs(b.leftVar, a.value)
      Some(Unary(b.rightVar, newQuery))
    }
    case _ => None 
  }
}

object UnaryIntersect extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (u1: Unary, u2: Unary) => {
      val newVar = u1.freeVar
      val oldVar = u2.freeVar
      val newQuery = u1.query.combine(u2.query.subs(oldVar, newVar))
      Some(Unary(newVar, newQuery))
    }
    case _ => None
  }
}

object UnaryIdentity extends Combinator {
  override def apply(left: Category, right: Category) = (left, right) match {
    case (id: Identity, u: Unary) => Some(u)
    case (u: Unary, id: Identity) => Some(u)
    case _ => None
  }
}

object MyTest extends App {
  
  val q = ListConjunctiveQuery.fromString("($x, capital of, $y").get
  val t = ListConjunctiveQuery.fromString("($x, is a, city)").get
  val a = UnquotedTLiteral("france")
  
  val binary = Binary(TVariable("x"), TVariable("y"), q)
  val unary = Unary(TVariable("x"), t)
  val arg = Arg(a)
  
  println(RightApply(binary, arg))
  println(LeftApply(arg, binary))
  
  println(RightApply(arg, binary))
  
  val thing = for {
    u <- RightApply(binary, arg)
    z <- UnaryIntersect(unary, u)
  } yield z
  println(thing)
  
  val thing2 = for {
    u <- RightApply(binary, arg)
    z <- UnaryIntersect(u, unary)
  } yield z
  println(thing2)
  
}