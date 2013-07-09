package edu.knowitall.triplestore
import org.scalatest.FlatSpec


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

}