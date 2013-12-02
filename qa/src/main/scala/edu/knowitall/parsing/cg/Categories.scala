package edu.knowitall.parsing.cg

import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.ConjunctiveQuery

trait Category

case class Arg(value: TLiteral) extends Category

case class Unary(freeVar: TVariable, query: ConjunctiveQuery) extends Category

case class Binary(leftVar: TVariable, rightVar: TVariable, 
    query: ConjunctiveQuery) extends Category

case class RelMod(value: String) extends Category
    
case class Identity() extends Category