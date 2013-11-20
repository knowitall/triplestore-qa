package edu.knowitall.search.qa

import edu.knowitall.collection.immutable.Interval

case class AbstractedArgState(question: String, tokens: IndexedSeq[String], 
     argInterval: Interval) extends QaState { 
  override def toString() = {
    val left = tokens.slice(0, argInterval.start)
    val middle = "[" +: tokens.slice(argInterval.start, argInterval.end) :+ "]"
    val right = tokens.slice(argInterval.end, tokens.size)
    val s = (left ++ middle ++ right).mkString(" ")
    s"AbstractedArgState($s)"
  } 
  def queryString = {
    val left = tokens.slice(0, argInterval.start)
    val right = tokens.slice(argInterval.end, tokens.size)
    (left ++ Seq("$y") ++ right).mkString(" ")
  }
  def arg = tokens.slice(argInterval.start, argInterval.end).mkString(" ")
}