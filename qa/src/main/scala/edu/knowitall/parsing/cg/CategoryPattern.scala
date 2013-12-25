package edu.knowitall.parsing.cg

import edu.knowitall.execution.TVariable
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral

trait CategoryPattern {
  def apply(bindings: Map[TVariable, String]): Category
}

case class UnaryPattern(pattern: String) extends CategoryPattern {
  private val cqp = ConjunctiveQueryPattern(pattern)
  assume(cqp.boundVars.size == 1, s"UnaryPattern $pattern must have 1 bound variable")
  override def apply(bindings: Map[TVariable, String]) = 
    Unary(freeVar = cqp.query.qVars(0), cqp(bindings))
}

case class BinaryPattern(pattern: String) extends CategoryPattern {
  private val cqp = ConjunctiveQueryPattern(pattern)
  assume(cqp.boundVars.size == 2, s"BinaryPattern $pattern must have 2 bound variables")
  override def apply(bindings: Map[TVariable, String]) = 
    Binary(leftVar = cqp.query.qVars(0), rightVar = cqp.query.qVars(1), cqp(bindings))
}

case class ArgumentPattern(pattern: String) extends CategoryPattern {
  private val sp = StringPattern(pattern)
  override def apply(bindings: Map[TVariable, String]) = Arg(UnquotedTLiteral(sp(bindings)))
}

case class RelModPattern(pattern: String) extends CategoryPattern {
  private val sp = StringPattern(pattern)
  override def apply(bindings: Map[TVariable, String]) = RelMod(sp(bindings))
}

object IdentityPattern extends CategoryPattern {
  override def apply(bindings: Map[TVariable, String]) = Identity
}

case class ConjunctiveQueryPattern(pattern: String) {
  
  val query = ListConjunctiveQuery.fromString(pattern) match {
    case Some(x) => x
    case None => throw new IllegalArgumentException(s"Invalid pattern: $pattern")
  }
  
  val boundVars = query.qVars
    
  val freeVars = query.conjuncts.flatMap(_.vars).toSet -- boundVars
  
  def apply(bindings: Map[TVariable, String]) = {
    assume(freeVars.subsetOf(bindings.keys.toSet), s"Binding $bindings does not cover free variables in $query")
    query.subs(bindings map { case (tvar, s) => (tvar, UnquotedTLiteral(s)) })
  }
  
}

case class StringPattern(pattern: String) {
  private val varPat = "\\$[A-Za-z0-9]+".r
  private val parts = varPat.split(s" $pattern ").toList
  private val variables = varPat.findAllIn(pattern).toList map {
    case x => TVariable(x.slice(1, x.size))
  }
  def apply(bindings: Map[TVariable, String]) = {
    assume(variables.toSet.subsetOf(bindings.keys.toSet), s"Binding $bindings does not cover free variables in $pattern")
    val pieces = for {
      (string, variable) <- parts zip variables
      value = bindings(variable)
    } yield s"${string}${value}"
    pieces.mkString("").trim()
  }
}

object Foo extends App {
  val cat = BinaryPattern("$x, $y : ($x, $1, $y)")
  val result = cat(Map(TVariable("1") -> "was born in"))
  println(result)
}