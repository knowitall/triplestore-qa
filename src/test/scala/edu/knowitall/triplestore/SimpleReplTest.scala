package edu.knowitall.triplestore
import org.scalatest.FlatSpec
import edu.knowitall.execution.Tabulator.{tuplesToTable => toTable}
import edu.knowitall.execution.QueryNode
import edu.knowitall.execution.Joiner
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.TConjunct


class SimpleReplTest extends FlatSpec {
  
  val sq = TConjunct.fromString("r", "$x, is, fruit")
  "AbstractQuery" should "parse correctly" in {
    val s = sq match {
      case Some(q) => q.toString
      case None => ""
    }
    assert("($x, is, fruit)" === s)
  }
  it should "identify literals" in {
    val n = sq match {
      case Some(q) => q.literalFields.size
      case _ => 0
    }
    assert(2 === n)
  }
  
  val input = """($x, eats, $y) ($y, is, fruit)"""
  val sqs = TConjunct.fromStringMult(input).toList.sortBy(_.toString)
  it should "handle multiple query input" in {
    assert(2 === sqs.size)
    assert("($x, eats, $y)" === sqs(0).toString)
    assert("($y, is, fruit)" === sqs(1).toString)
  }
  
  val varx = TVariable("x")
  val vary = TVariable("y")
  
  val sq2 = TConjunct.fromString("r", "$x, is made from, $y")
  "QueryNode" should "identify join attrs" in {
    val node = QueryNode(sq2.get)
    val attrs = node.joinAttrs
    val expected = Map(varx -> List("r.arg1"), vary -> List("r.arg2"))
    assert(expected === attrs)
  }
  
  
  val sq3 = TConjunct.fromString("s", "$y, high in, antioxidants")
  it should "merge join attrs" in {
    val node2 = QueryNode(sq2.get)
    val node3 = QueryNode(sq3.get)
    val attrs2 = node2.joinAttrs
    val attrs3 = node3.joinAttrs
    val merged = Joiner.mergeJoinAttrs(attrs2, attrs3)
    val expected = Map(varx -> List("r.arg1"), vary -> List("r.arg2", "s.arg1"))
    assert(expected === merged)
  }
  

}