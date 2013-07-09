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

trait QVal

case class Literal(value: String) extends QVal {
  override def toString = value
}

case class Variable(name: String) extends QVal {
  override def toString = s"name"
}

case class AbstractQuery(values: Map[Field, QVal]) {  
  def literalFields: Iterable[(Field, String)] = { 
    for ((f, v) <- values) yield v match {
      case Literal(s) => Some((f, s))
      case _ => None
    }
  }.flatten
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
  val vpat = """$(.*)""".r
  def fromString(s: String): Option[AbstractQuery] = s match {
    case qpat(x, r, y) => Some(fromTriple(x, r, y))
    case _ => None
  }
  def getQVal(s: String): QVal = s match {
    case vpat(v) => Variable(v)
    case _ => Literal(s)
  }
  val fields = List(arg1, rel, arg2)
  def fromTriple(x: String, r: String, y: String): AbstractQuery = {
    val lst = List(x.trim(), r.trim(), y.trim())
    val items = for ((f, a) <- fields.zip(lst); v = getQVal(a)) yield (f, v)
    AbstractQuery(items.toMap)
  }
  val splitPat = """(?<=\))\s*?(?=\()"""
  def fromStringMult(s: String): Iterable[AbstractQuery] = 
    s.split(splitPat).flatMap(fromString(_))
} 

case class JoinNode(name: String) {
  override def toString = name
}

case class JoinEdge(node1: JoinNode, node2: JoinNode, pred: TuplePred) {
  def incident(node: JoinNode): Boolean = node == node1 || node == node2
  def incident(n1: JoinNode, n2: JoinNode): Boolean = 
    incident(n1) && incident(n2)
}

case class JoinGraph(nodes: Set[JoinNode], edges: Set[JoinEdge]) {
  
  def other(e: JoinEdge, n1: JoinNode, n2: JoinNode) = {
    val u1 = e.node1
    val u2 = e.node2
    if (u1 != n1 && u1 != n2) {
      u1
    } else {
      u2
    }
  }
  

  def collapse(n1: JoinNode, n2: JoinNode): JoinGraph = {
    val newNode = JoinNode(s"($n1*$n2)")
    val newEdges = edges.flatMap(e => {
      if (e.incident(n1, n2)) {
        None
      } else if (e.incident(n1)) {
        Some(JoinEdge(newNode, n2, e.pred))
      } else if (e.incident(n2)) {
        Some(JoinEdge(newNode, n1, e.pred))
      } else {
        Some(e)
      }
    })

    //val mergeEdges = edges.filter(e => e.incident(n1) ^ e.incident(n2))
    
    null
  }
  
  val truep = (t: Tuple) => true 
  def andPair(p1: TuplePred, p2: TuplePred) = (t: Tuple) => p1(t) && p2(t)
  def and(ps: TuplePred*): TuplePred = ps.foldLeft(truep)(andPair)
}


/*
case class AbstractQuery(arg1: QVal, rel: QVal, arg2: QVal) {
  def query: Query = {
    var query = Seq[Query]()
	if (arg1 != Variable) query = Arg1Cont(arg1.toString()) +: query
	if (rel != Variable) query = RelCont(rel.toString()) +: query
    if (arg2 != Variable) query = Arg2Cont(arg2.toString()) +: query
    return Conjunction(query:_*)
  }
  def joinCount = List(arg1, rel, arg2).count(_ == Variable)
}
case object AbstractQuery {
  val pat = """\(?(.+),(.+),(.+?)\)?""".r
  def toQval(s: String) = s.trim() match {
    case "?" => Variable
    case _ => Literal(s)
  }
  def fromString(s: String): Option[AbstractQuery] = s match {
    case pat(x, r, y) => Some(AbstractQuery(toQval(x), toQval(r), toQval(y)))
    case _ => None
  } 
}

case class JoinGroup(queries: IndexedSeq[AbstractQuery]) {
  assert(queries.forall(_.joinCount == 1))
  def size = queries.size
  val names = {for (i <- 1 to size) yield s"r$i"}.toIndexedSeq
  def joinCol(i: Integer) = {
    val q = queries(i)
    val j = i + 1
    if (q.arg1 == Variable) s"r$j.arg1"
    else s"r$j.arg2"
  }
  val projCols = { for (t <- names) yield List(s"$t.arg1", s"$t.rel", s"$t.arg2", s"$t.namespace") }.toList.flatten
}

case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", hits: Int = 100) {
  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._

  def count(q: AbstractQuery): Long = client.count(q.query)
  
  def evalJoin(qs: IndexedSeq[AbstractQuery]) = {
    val sorted = qs.sortBy(count(_))
    val grp = JoinGroup(sorted)
    var res = SearchFor(grp.names(0), sorted(0).query).toList
    for (i <- Range(1, grp.size)) {
      val k1 = grp.joinCol(i-1)
      val k2 = grp.joinCol(i)
      val pq = sorted(i).query
      res = SearchJoin(k1, k2, res, PartialSearchFor(grp.names(i), pq)).toList
    }
    val projection = On(grp.projCols:_*)
    res = Project(projection, res).toList
    println(toTable(res))
  }
  
  val splitPat = """(?<=\))\s*?(?=\()"""
  def parseInput(input: String): Iterable[AbstractQuery] = 
    input.split(splitPat).flatMap(AbstractQuery.fromString(_))
  
  def eval2(input: String) = {
    val qs = parseInput(input).toIndexedSeq
    if (qs.size > 1) {
      evalJoin(qs)
    } else if (qs.size == 1) {
      evalQ(qs(0))
    } else {
      println("Error.")
    }
  }  
    
  def eval(input: String) = for (q <- parseInput(input)) {
    println(count(q) + " hits")
    evalQ(q)
  }
    
  def evalQ(q: AbstractQuery) = println(toTable(search(q)))
  
  val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace")
  def search(q: AbstractQuery) = Project(projection, SearchFor("r", q.query))

  
}

object SimpleRepl extends Application {
  
  val repl = SimpleRepl()
  val reader = new ConsoleReader()
  while (true) {
	  val line = reader.readLine("> ")
	  repl.eval2(line)
	  println
  }

}*/