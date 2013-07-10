package edu.knowitall.triplestore
import Search.PartialSearchJoin
import PartialFunction._
import Search.{Arg1Cont, RelCont, Arg2Cont}
import Search.Field._
import Search.Query
import Conditions.TuplePred
import Operators.{Select, Product}
import edu.knowitall.triplestore.Search.Conjunction
import edu.knowitall.triplestore.Search.FieldKeywords
import edu.knowitall.triplestore.Conditions.AttrsSim
import Operators.NestedLoopJoin

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
  
  def joinKeys: Map[Variable, String] = {
    val vfs: Map[Variable, Field] = varsToFields
    val pairs = for (v <- vfs.keys; f <- vfs.get(v); a = name + "." + f)
      yield (v, a)
    pairs.toMap
  }
  
  def vars: Iterable[Variable] = {
    varsToFields.keys.toSet
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

trait TableNode {
  val joinAttrs: Map[Variable, List[String]]
  def getJoinAttrs(v: Variable): List[String]
  def hasVariable(v: Variable) = joinAttrs.contains(v)
}

case class QueryNode(absQuery: AbstractQuery) extends TableNode {
  val jks = absQuery.joinKeys
  val joinAttrs = { 
    for (v <- jks.keys; attr <- jks.get(v)) yield (v, List(attr)) 
  }.toMap
  def getJoinAttrs(v: Variable): List[String] = joinAttrs.get(v) match {
    case Some(v) => v
    case _ => List[String]()
  }
}

case class TuplesNode(tuples: List[Tuple], 
    joinAttrs: Map[Variable, List[String]]) extends TableNode {
  def getJoinAttrs(v: Variable): List[String] = joinAttrs.get(v) match {
    case Some(v) => v
    case _ => List[String]()
  }
}

case class Joiner(client: TriplestoreClient) {
  val planning = TriplestorePlan(client)
  import planning._
  def cost(n: TableNode) = n match {
    case q: QueryNode => client.count(q.absQuery.partialQuery)
    case t: TuplesNode => t.tuples.size
  }

  def joinTT(tn1: TuplesNode, tn2: TuplesNode, v: Variable): Tuples = {
    val attrPairs = for(a1 <- tn1.getJoinAttrs(v); a2 <- tn2.getJoinAttrs(v))
      yield (a1, a2)
    val pred = Joiner.pairsToCond(attrPairs)
    NestedLoopJoin(pred)(tn1.tuples, tn2.tuples)
  }
  
  def joinQT(qn: QueryNode, tn: TuplesNode, v: Variable): Tuples = {
    val attrPairs = for (
        a1 <- tn.getJoinAttrs(v);
        a2 <- qn.getJoinAttrs(v)) yield (a1, a2)
        
        
    val bpred = attrPairs match {
      case (a1, a2) :: tail => AttrsSim(a1, a2, 0.9) 
      case _ => AttrsSim("", "", 0.0)
    }
    val spred = attrPairs match {
      case (a1, a2) :: tail => Joiner.pairsToCond(tail)
      case _ => Joiner.truep
    }
    val left = tn.tuples
    val right = PartialSearchFor(qn.absQuery.name, qn.absQuery.partialQuery)
    val joined = PartialSearchJoin(bpred)(left, right)
    Select(spred)(joined)
  }
  
  def joinQueries(queries: Iterable[AbstractQuery]): Tuples = {
    val nodes = queries.map(QueryNode(_)).toList
    join(nodes)
  }
  
  val emptyTuples = List(Tuple(Map()))
  val prod = (ts1: Iterable[Tuple], ts2: Iterable[Tuple]) => Product(ts1, ts2).toList
  def join(nodes: List[TableNode]): Iterable[Tuple] = {
    val joined = mergeLowest(nodes).map(toTuplesNode(_))
    joined.map(_.tuples).foldLeft(emptyTuples)(prod).toList
  }
  
  def mergeLowest(nodes: List[TableNode]): List[TableNode] = { 
    val merged = lowestVariable(nodes) match {
      case Some(v) => mergeLowest(groupThenMergeNodes(nodes, v))
      case None => nodes
    }
    merged.map(toTuplesNode(_))
  }
  
  def groupThenMergeNodes(nodes: List[TableNode], v: Variable): List[TableNode] = {
    println("MERGING LOWEST = " + v)
    val (toMerge, toKeep) = nodes.partition(_.hasVariable(v))
    mergeNodes(toMerge, v) +: toKeep
  }
  
  def lowestVariable(nodes: List[TableNode]): Option[Variable] = {
    val lst = for (n <- nodes; v <- n.joinAttrs.keySet) yield (v, n)
    val varNodes = lst.groupBy(e => e._1).mapValues(e => e.map(x => x._2).toSet)
    val varCosts = { for ((v, nodes) <- varNodes;
         if nodes.size > 1;
         costs = nodes.map(cost(_));
         minCost = costs.min) yield (v, minCost) }.toMap
    if (varCosts.size > 0) {
      val vars = varCosts.keys
      Some(vars.minBy(varCosts(_)))
    } else {
      None
    }
  } 
  
  def mergeNodes(nodes: List[TableNode], v: Variable): TuplesNode = {
    val node = eliminateVar(nodes, v)
    TuplesNode(node.tuples, node.joinAttrs-v)
  }

  def eliminateVar(nodes: List[TableNode], v: Variable): TuplesNode = {
    nodes match {
      case node :: Nil => toTuplesNode(node)
      case node1 :: node2 :: rest => eliminateVar(doJoin(node1, node2, v) :: rest, v)
      case _ => throw new IllegalArgumentException("empty node list")
    }
  }
  
  def doJoin(n1: TableNode, n2: TableNode, v: Variable): TuplesNode = {
    val t = toTuplesNode(n1)
    val tuples = n2 match {
      case q: QueryNode => joinQT(q, t, v)
      case t2: TuplesNode => joinTT(t, t2, v)
    }
    val merged = Joiner.mergeJoinAttrs(n1.joinAttrs, n2.joinAttrs)
    TuplesNode(tuples.toList, merged)
  }
  
  def toTuplesNode(node: TableNode): TuplesNode = node match {
    case t: TuplesNode => t
    case q: QueryNode => queryToTuples(q)
    case _ => throw new IllegalArgumentException("invalid node type: " + node)
  }
  
  def queryToTuples(q: QueryNode): TuplesNode = {
    val tuples = SearchFor(q.absQuery.name, q.absQuery.partialQuery)
    TuplesNode(tuples, q.joinAttrs)
  }

}
case object Joiner {
  type JA = Map[Variable, List[String]]
  val eqCond = (a1: String, a2: String) => AttrsSim(a1, a2, 0.9)
  def mergeJoinAttrs(attrs1: JA, attrs2: JA): JA = {
    val allVars = attrs1.keySet union attrs2.keySet
    val e: List[String] = List[String]()
    val newPairs = for (v <- allVars;
         as1 = attrs1.getOrElse(v, e).toSet;
         as2 = attrs2.getOrElse(v, e).toSet)
      yield (v, (as1 ++ as2).toList)
    return newPairs.toMap
  }
  val truep = (t: Tuple) => true
  def and(p1: TuplePred, p2: TuplePred) = (t: Tuple) => p1(t) && p2(t) 
  def andList(preds: Iterable[TuplePred]): TuplePred = 
    preds.foldLeft(truep)(and)
  def pairsToCond(pairs: List[(String, String)]): TuplePred = {
    val preds = for ((a1, a2) <- pairs) yield eqCond(a1, a2)
    andList(preds)
  }
  
}