package edu.knowitall.triplestore
import org.scalatest.FlatSpec
import Tabulator.{tuplesToTable => toTable}


class SimpleReplTest extends FlatSpec {
  
  val sq = AbstractQuery.fromString("r", "$x, is, fruit")
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
  val sqs = AbstractQuery.fromStringMult(input).toList.sortBy(_.toString)
  it should "handle multiple query input" in {
    assert(2 === sqs.size)
    assert("($x, eats, $y)" === sqs(0).toString)
    assert("($y, is, fruit)" === sqs(1).toString)
  }
  
  val varx = Variable("x")
  val vary = Variable("y")
  
  val sq2 = AbstractQuery.fromString("r", "$x, is made from, $y")
  "QueryNode" should "identify join attrs" in {
    val node = QueryNode(sq2.get)
    val attrs = node.joinAttrs
    val expected = Map(varx -> List("r.arg1"), vary -> List("r.arg2"))
    assert(expected === attrs)
  }
  
  
  val sq3 = AbstractQuery.fromString("s", "$y, high in, antioxidants")
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