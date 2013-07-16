package edu.knowitall.execution

import org.scalatest.FlatSpec

class ConjunctiveQueryTest extends FlatSpec {
  
  val q = "$x : ($x, contains, potassium)"
  
  "ListConjunctiveQuery" should "parse strings correctly" in {
    val result = ListConjunctiveQuery.fromString(q) match {
      case Some(cq: ListConjunctiveQuery) => cq
      case _ => throw new IllegalArgumentException("")
    }
    assert(TVariable("x") === result.qVar)
    assert(1 === result.conjuncts.size)
  }

}