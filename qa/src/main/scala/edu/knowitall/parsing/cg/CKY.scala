package edu.knowitall.parsing.cg

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.collection.immutable.Interval
import scala.collection.mutable.{Map => MutableMap}

trait Node {
  def span: Interval
  def category: Category
}

case class CatSpan(category: Category, span: Interval)

case class Terminal[T](catspan: CatSpan,
					   rule: TerminalRule[T]) extends Node {
  override val span = catspan.span
  override val category = catspan.category
}

case class NonTerminal(catspan: CatSpan, left: CatSpan, right: CatSpan, 
    rule: Combinator) extends Node {
  override val span = catspan.span
  override val category = catspan.category
}

case class CKY[T](input: T, size: Int, 
				  terminalRules: IndexedSeq[TerminalRule[T]],
				  combinators: IndexedSeq[Combinator]) {
  
  val cats = MutableMap.empty[Interval, Category]
  val nodes = MutableMap.empty[CatSpan, Node]
  
  private def intervals(length: Int) = for {
    i <- 0 until (size - length + 1)
  } yield Interval.open(i, i + length)
    
  private def allIntervals = for {
    length <- 1 to size
    interval <- intervals(length)
  } yield interval
  
  private def splits(interval: Interval) = for {
    k <- (interval.head + 1) until interval.end
    i = interval.head
    j = interval.end
  } yield (Interval.open(i, k), Interval.open(k, j))
  
  private def applyTerminalRules = for {
    interval <- allIntervals
    rule <- terminalRules
    category <- rule(interval, input)
    catspan = CatSpan(category, interval)
    terminal = Terminal(catspan, rule)
  } yield {
    cats += (interval -> category)
    nodes += (catspan -> terminal)
  }
  
  private def applyCombinators(length: Int) = for {
    interval <- intervals(length)
    (left, right) <- splits(interval)
    lcat <- cats.get(left)
    rcat <- cats.get(right)
    combinator <- combinators
    cat <- combinator(lcat, rcat)
    node = NonTerminal(CatSpan(cat, interval), CatSpan(lcat, left), CatSpan(rcat, right), combinator)
  } {
    cats += (interval -> cat)
    nodes += (CatSpan(cat, interval) -> node)
  }
  
  private def parse = {
    applyTerminalRules
    for (length <- 2 to size) applyCombinators(length)
  }
  
}