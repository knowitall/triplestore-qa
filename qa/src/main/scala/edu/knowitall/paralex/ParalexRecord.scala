package edu.knowitall.paralex

import scala.io.Source
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JDouble
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.Search
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.SimpleQuery
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal

case class ParalexRecord(query: ConjunctiveQuery, score: Double)

object ParalexRecord extends App  {
  
  def getFieldNamed(v: JValue, n: String) = v match {
    case f: JField if f.name == n => Some(f.value)
    case _ => None
  }
  
  def getStringNamed(v: JValue, n: String) = getFieldNamed(v, n) match {
    case Some(JString(s)) => Some(s)
    case _ => None
  }
  
  def getDoubleNamed(v: JValue, n: String) = getFieldNamed(v, n) match {
    case Some(JDouble(d)) => Some(d)
    case _ => None
  }
  
  def getField(f: String) = f match {
    case "arg1" => Some(Search.arg1)
    case "rel" => Some(Search.rel)
    case "arg2" => Some(Search.arg2)
    case _ => None
  }
  
  def toLiteral(s: String) = UnquotedTLiteral(s.replaceAll("-", " ").replaceAll("""\.[er]$""", ""))
  
  def makeQuery(s: String, f1: String, v1: String, f2: String, v2: String) = 
    for {
      projectField <- getField(s)
      projectVar = TVariable("x")
      whereField1 <- getField(f1)
      whereConst1 = toLiteral(v1)
      whereField2 <- getField(f2)
      whereConst2 = toLiteral(v2)
      fields = Map[Field, TVal](projectField -> projectVar,
          whereField1 -> whereConst1, 
          whereField2 -> whereConst2)
    } yield SimpleQuery("r0", fields)
  
  val selectPattern = """SELECT (arg[12]) FROM tuples WHERE ([a-z0-9]+)="([^"]*)" AND ([a-z0-9]+)="([^"]*)"""".r
  def parseQuery(s: String) = s match {
    case selectPattern(s, f1, v1, f2, v2) => makeQuery(s, f1, v1, f2, v2)
    case _ => None
  }
  
  def getRecords(v: JValue) = for {
    score <- v.children.flatMap(getDoubleNamed(_, "score"))
    queryString <- v.children.flatMap(getStringNamed(_, "query"))
    query <- parseQuery(queryString)
  } yield ParalexRecord(query, score)
  
  def isRecord(v: JValue) = !getRecords(v).isEmpty
  
  def fromJson(s: String) = {
    val obj = parse(s)
    obj.filter(isRecord).flatMap(getRecords)
  }
  
}