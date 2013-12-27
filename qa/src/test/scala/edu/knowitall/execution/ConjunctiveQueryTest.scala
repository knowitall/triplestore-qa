package edu.knowitall.execution

import org.scalatest.FlatSpec
import edu.knowitall.execution.Search.arg1

class ConjunctiveQueryTest extends FlatSpec {
  
  "ListConjunctiveQuery" should "parse strings correctly" in {
    val q = "$x : ($x, contains, potassium)"
    val result = ListConjunctiveQuery.fromString(q) match {
      case Some(cq: ListConjunctiveQuery) => cq
      case _ => throw new IllegalArgumentException("")
    }
    assert(List(TVariable("x")) === result.qVars)
    assert(1 === result.conjuncts.size)
  }
  
  it should "handle joins" in {
    val q = """$x : (salad, with, $x) ($x, source of, protein) ($x, compatible with, vegetarians)"""
    val result = ListConjunctiveQuery.fromString(q) match {
      case Some(cq: ListConjunctiveQuery) => cq
      case _ => throw new IllegalArgumentException("")
    }
    assert(List(TVariable("x")) === result.qVars)
    assert(3 === result.conjuncts.size)
  }

}