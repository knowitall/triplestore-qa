package edu.knowitall.parsing.cg

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.collection.immutable.Interval
import scala.collection.mutable.{Map => MutableMap}
import edu.knowitall.util.MathUtils

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
  
  val cats = MutableMap.empty[Interval, Set[Category]]
  val nodes = MutableMap.empty[CatSpan, Node]
  
  private def applyTerminalRules = for {
    interval <- MathUtils.allIntervals(size)
    rule <- terminalRules
    category <- rule(interval, input)
    catspan = CatSpan(category, interval)
    terminal = Terminal(catspan, rule)
  } yield {
    cats += (interval -> (cats.getOrElse(interval, Set.empty) + category))
    nodes += (catspan -> terminal)
  }
  
  private def applyCombinators(length: Int) = for {
    interval <- MathUtils.intervals(length, size)
    (left, right) <- MathUtils.splits(interval)
    lcat <- cats.getOrElse(left, Set.empty)
    rcat <- cats.getOrElse(right, Set.empty)
    combinator <- combinators
    cat <- combinator(lcat, rcat)
    node = NonTerminal(CatSpan(cat, interval), CatSpan(lcat, left), CatSpan(rcat, right), combinator)
  } {
    cats += (interval -> (cats.getOrElse(interval, Set.empty) + cat))
    nodes += (CatSpan(cat, interval) -> node)
  }
  
  def parse = {
    applyTerminalRules
    for (length <- 2 to size) applyCombinators(length)
  }
  
  private val fullSpan = Interval.open(0, size)
  
  def rootCategories = cats.getOrElse(fullSpan, Set())

  
}