package edu.knowitall.triplestore
import org.scalatest.FlatSpec


class SimpleReplTest extends FlatSpec {
  
  "AbstractQuery" should "parse correctly" in {
    val sq = AbstractQuery.fromString("$x, is, fruit")
    val s = sq match {
      case Some(q) => q.toString
      case None => ""
    }
    assert("($x, is, fruit)" === s)
  }

}