package edu.knowitall.execution
package generalize

import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.ClearPostagger
import org.slf4j.LoggerFactory

class RuleGeneralizingExecutor(val baseExecutor: QueryExecutor) extends GeneralizingExecutor {

  private val logger = LoggerFactory.getLogger(this.getClass)
  
  val postagger = new ClearPostagger()
  
  def postag(s: String) = postagger.synchronized( postagger.postag(s) )
  
  def isAdverb(t: PostaggedToken) = t.postag.startsWith("RB")
  
  def isModifier(t: PostaggedToken): Boolean = t.isAdjective || isAdverb(t)
  
  def isPreposition(t: PostaggedToken): Boolean = t.isPreposition
  
  def strip(s: String, filter: PostaggedToken => Boolean): String = {
    val nonModifiers = postag(s).filterNot(filter)
    nonModifiers.map(_.string).mkString(" ")
  }
  
  def strip(conj: TConjunct, filter: PostaggedToken => Boolean): TConjunct = {
    val modifiersStripped = conj.values.map { 
      case (field, UnquotedTLiteral(value)) => 
        val strippedValue = strip(value, filter)
        val dontEmpty = if (strippedValue.isEmpty) value else strippedValue
        (field, UnquotedTLiteral(dontEmpty))
      case x => x
    }
    conj.copy(values = modifiersStripped)
  }
  
  def strip(q: ConjunctiveQuery, filter: PostaggedToken => Boolean): ConjunctiveQuery = {
    ListConjunctiveQuery(q.qVars, q.conjuncts map { c => strip(c, filter) })
  }
  
  def stripLeftmost(s: String): String = s.split(" ").drop(1).mkString(" ")
  
  def stripLeftmost(conj: TConjunct): Option[TConjunct] = {
    // find the longest literal
    val lengths = conj.values.map {
      case (field, literal: UnquotedTLiteral) => 
        (field, (literal, postag(literal.value).length))
      case (field, value) => (field, (value, 0))
    }
    val maxLength = lengths.maxBy(_._2._2)._2._2
    // if maxLength is less than one, removing would yield an empty literal.
    if (maxLength <= 1) None
    else {
      val (field, (lit, len)) = lengths.find(_._2._2.equals(maxLength)).get
      // safe because length must be > 0 ...
      val leftmostStripped = UnquotedTLiteral(stripLeftmost(lit.asInstanceOf[UnquotedTLiteral].value))
      val newValues = conj.values + (field -> leftmostStripped)
      Some(conj.copy(values = newValues))
    }
  }
  
  def stripModifiers(q: ConjunctiveQuery) = strip(q, isModifier _)
  
  def stripPrepositions(q: ConjunctiveQuery) = strip(q, isPreposition _)
  
  def stripLeftmost(q: ConjunctiveQuery): Option[ConjunctiveQuery] = {
    
    val conjunctOpts = q.conjuncts map stripLeftmost
    conjunctOpts.find(_.isEmpty) match {
      case Some(empty) => None
      case None => 
        val conjunctsGet = conjunctOpts.map(_.get)
        Some(ListConjunctiveQuery(q.qVars, conjunctsGet))
    }
  }
  
  override def generalizations(q: ConjunctiveQuery): Iterator[ConjunctiveQuery] = {
    val noModifiers = stripModifiers(q)
    val noPreps = stripPrepositions(noModifiers)
    
    logger.info(s"generalizations($q)")
    
    var leftmosts = Seq.empty[ConjunctiveQuery]
    var strippedLeft = stripLeftmost(noPreps)
    var last: Option[ConjunctiveQuery] = None
    while (!strippedLeft.isEmpty && strippedLeft != last) {
      leftmosts = leftmosts ++ Seq(strippedLeft.get)
      last = strippedLeft
      strippedLeft = stripLeftmost(noPreps)
    }
    (Seq(noModifiers, noPreps) ++ leftmosts).distinct.iterator
  }
}