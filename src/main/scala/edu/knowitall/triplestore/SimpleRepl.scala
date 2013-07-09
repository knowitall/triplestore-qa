package edu.knowitall.triplestore
import PartialFunction._
import Search.{Arg1Cont, RelCont, Arg2Cont}
import Search.Field._
import Search.Query
import Conditions.TuplePred
import Tabulator.{tuplesToTable => toTable}
import jline.console.ConsoleReader
import edu.knowitall.triplestore.Search.Conjunction
import edu.knowitall.triplestore.Search.FieldKeywords
import edu.knowitall.triplestore.Conditions.AttrsSim

trait QVal

case class Literal(value: String) extends QVal {
  override def toString = value
}

case class Variable(name: String) extends QVal {
  override def toString = "$" + name
}

case class AbstractQuery(name: String, values: Map[Field, QVal]) {  
  
  def literalFields: Iterable[(Field, String)] = { 
    for ((f, v) <- values) yield v match {
      case Literal(s) => Some((f, s))
      case _ => None
    }
  }.flatten
  
  def variableFields: Iterable[(Field, Variable)] = {
    for ((f, v) <- values) yield v match {
      case Variable(s) => Some((f, Variable(s)))
      case _ => None
    }
  }.flatten
  
  def varsToFields: Map[Variable, Field] = variableFields.map(_.swap).toMap
  
  def partialQuery: Query = {
    val conjuncts = for ((f, v) <- literalFields) yield FieldKeywords(f, v)
    Conjunction(conjuncts.toList:_*)
  }
  val xs = values.getOrElse(arg1, "")
  val rs = values.getOrElse(rel, "")
  val ys = values.getOrElse(arg2, "")
  override def toString = s"($xs, $rs, $ys)"
}

case object AbstractQuery {
  val qpat = """\(?(.+),(.+),(.+?)\)?""".r
  val vpat = """\$(.*)""".r
  def fromString(name: String, s: String): Option[AbstractQuery] = s match {
    case qpat(x, r, y) => Some(fromTriple(name, x, r, y))
    case _ => None
  }
  def getQVal(s: String): QVal = s match {
    case vpat(v) => Variable(v)
    case "?" => Variable("?")
    case _ => Literal(s)
  }
  val fields = List(arg1, rel, arg2)
  def fromTriple(name: String, x: String, r: String, y: String): AbstractQuery = {
    val lst = List(x.trim(), r.trim(), y.trim())
    val items = for ((f, a) <- fields.zip(lst); v = getQVal(a)) yield (f, v)
    AbstractQuery(name, items.toMap)
  }
  val splitPat = """(?<=\))\s*?(?=\()"""
  def fromStringMult(s: String): Iterable[AbstractQuery] = {
    val parts = s.split(splitPat).toList
    for ((s, i) <- parts.zipWithIndex; q <- fromString(s"r$i", s)) yield q
  }
} 



case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", hits: Int = 100) {
  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._

  def count(q: AbstractQuery): Long = client.count(q.partialQuery)
  
  val splitPat = """(?<=\))\s*?(?=\()"""
  def parseInput(input: String): Iterable[AbstractQuery] =  
    input.split(splitPat).flatMap(AbstractQuery.fromStringMult(_))
  
  
  def eval2(input: String) = {
    val qs = parseInput(input).toIndexedSeq
    if (qs.size == 1) {
      println(count(qs(0)))
      evalQ(qs(0))
    } else {
      println("Error.")
    }
  } 
    
  def evalQ(q: AbstractQuery) = println(toTable(search(q)))
  
  val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace")
  def search(q: AbstractQuery) = Project(projection, SearchFor("r", q.partialQuery))

  
}

object SimpleRepl extends Application {
  
  val repl = SimpleRepl()
  val reader = new ConsoleReader()
  while (true) {
	  val line = reader.readLine("> ")
	  repl.eval2(line)
	  println
  }

}