package edu.knowitall.triplestore

import com.rockymadden.stringmetric.similarity._
import org.apache.solr.client.solrj.util.ClientUtils


case class Tuple(attrs: Map[String, Any]) {
    
  def join(other: Tuple): Tuple = {
    val t = Tuple(this.attrs ++ other.attrs)
    if (t.attrs.size == this.attrs.size + other.attrs.size) {
      return t
    } else {
      throw new IllegalArgumentException("attr names not disjoint: " + this + ", " + other)
    }
  }
  
  def get(a: String) = attrs.get(a)
  
  def getString(a: String) = attrs.get(a) match {
    case Some(x: String) => Some(x)
    case _ => None
  }
  
  def rename(f: String => String): Tuple = {
    Tuple(attrs.map{ case (k, v) => (f(k), v) })
  }
  
  def renamePrefix(p: String): Tuple = rename(k => p + "." + k)
  
  override def toString: String = {
    val pairs = attrs map { case (k, v) => k + ": " + v }
    return "(" + pairs.mkString(", ") + ")"
  }
}

object StrSim {
  val stops = Set("a", "an", "and", "are", "as", "at", "be", "but", "by",
      "for", "if", "in", "into", "is", "it",
      "no", "not", "of", "on", "or", "such",
      "that", "the", "their", "then", "there", "these",
      "they", "this", "to", "was", "will", "with", "i", "me", "your",
      "our", "ours", "him", "he", "his", "her", "its", "you", "that",
      "every", "all", "each", "those", "other", "both", "neither", "some",
      "'s")
  
  def norm(x: String) = {
    x.toLowerCase().split("\\s+").filter(!stops.contains(_)).mkString("")
  }
  def sim(x: String, y: String): Double = {
    JaroWinklerMetric.compare(norm(x), norm(y)).getOrElse(0.0)
  }
      
}

object Conditions {
  

  type Attr = String
  type Value = Any
  type ValuePred = Value => Boolean
  type BinaryStringPred = (String, String) => Boolean
  type BinaryValuePred = (Value, Value) => Boolean
  type TuplePred = Tuple => Boolean
  
    
  def valPair(a1: Attr, a2: Attr, t: Tuple) = for (v1 <- t.attrs.get(a1); v2 <- t.attrs.get(a2)) yield (v1, v2)
  
  def binaryAttrPred(a1: Attr, a2: Attr, f: BinaryValuePred): TuplePred = (t: Tuple) => {
    valPair(a1, a2, t).exists(f.tupled)
  }
  
  def unaryAttrPred(a: Attr, x: String, f: BinaryValuePred): TuplePred = (t: Tuple) => {
    { for (v <- t.attrs.get(a)) yield (v, List(x)) }.exists(f.tupled)
  }
  
  def stringToAnyPred(f: BinaryStringPred): BinaryValuePred = (v1: Value, v2: Value) => {
    (v1, v2) match {
      case (v1: String, v2: String) => f(v1, v2)
      case _ => false
    }
  }
  
  def binaryPredFromString(a1: Attr, a2: Attr, f: BinaryStringPred) = {
    binaryAttrPred(a1, a2, stringToAnyPred(f))
  }
  
  def unaryPredFromString(a: Attr, v: String, f: BinaryStringPred) = {
    unaryAttrPred(a, v, stringToAnyPred(f))
  }

  type Synonyms = List[List[String]]
  def inSameSet(syns: Synonyms) = (x: String, y: String) => 
    x == y || syns.exists(s => s.contains(x) && s.contains(y))
  
  def StringEquality(caseSensitive: Boolean = true) = {
    (x: String, y: String) => {
      if (caseSensitive) x == y else x.toLowerCase() == y.toLowerCase() 
    }
  }    
    
  
  def strEq = StringEquality(false)
  def valEq = (x: Value, y: Value) => x == y
  def strSim(thresh: Double) = (x: String, y: String) => StrSim.sim(x, y) > thresh

  trait BinaryPred {
    def attr1: Attr
    def attr2: Attr
    def apply(t: Tuple): Boolean
  }
  
  case class AttrsEqual(attr1: Attr, attr2: Attr) extends TuplePred with BinaryPred {
    val pred = binaryPredFromString(attr1, attr2, strEq)
    def apply(t: Tuple) = pred(t)
  }
  
  case class AttrEquals(attr: Attr, value: String) extends TuplePred {
    val pred = unaryPredFromString(attr, value, strEq)
    def apply(t: Tuple) = pred(t)
  }
  
  case class AttrsSim(attr1: Attr, attr2: Attr, thresh: Double) extends TuplePred with BinaryPred {
    val pred = binaryPredFromString(attr1, attr2, strSim(thresh))
    def apply(t: Tuple) = pred(t)
  }
  
  case class AttrSim(attr: Attr, value: String, thresh: Double) extends TuplePred {
    val pred = unaryPredFromString(attr, value, strSim(thresh))
    def apply(t: Tuple) = pred(t)
  }
    
  def On(attrs: Attr*) = (t: Tuple) => {
    val items = for (a <- attrs; v <- t.attrs.get(a)) yield (a, v)
    Tuple(items.toMap)
  }

}

object Operators {
  type TuplePred = Tuple => Boolean
  type TupleMap = Tuple => Tuple
  type Tuples = Iterable[Tuple]
  
  def Select(p: TuplePred) = (ts: Tuples) => ts.filter(p)
  
  def Project(m: TupleMap) = (ts: Tuples) => ts.map(m)
  
  def Union(tss: Tuples*) = tss.flatten
  
  def NestedLoopJoin(p: TuplePred) = (ts1: Tuples, ts2: Tuples) => {
    for (t1 <- ts1; t2 <- ts2; j = t1.join(t2); if p(j)) yield j
  }
  
  def Product(ts1: Tuples, ts2: Tuples): Tuples = for (t1 <- ts1; t2 <- ts2) yield t1.join(t2)
  
}

object Search {
  
  import Conditions._
  
  object Field extends Enumeration {
    type Field = Value
    val arg1, rel, arg2, namespace = Value
    val arg1_exact, rel_exact, arg2_exact = Value
  }
  import Field._
  
  trait Query {
    def toQueryString: String
  }
  
  def escape = ClientUtils.escapeQueryChars _
  
  case class FieldKeywords(f: Field, v: String) extends Query {
    def toQueryString = { for (w <- v.trim().split("\\s+"); x = f.toString() + ":" + escape(w)) yield x }.mkString(" AND ") 
  }
  
  case class FieldPhrase(f: Field, v: String) extends Query {
    def toQueryString = f.toString() + ":\"" + escape(v) + "\"" 
  }
  
  case class Conjunction(conjuncts: Query*) extends Query {
    def toQueryString = conjuncts.map("(" + _.toQueryString + ")").mkString(" AND ") 
  }

  case class Disjunction(disjuncts: Query*) extends Query {
    def toQueryString = disjuncts.map("(" + _.toQueryString + ")").mkString(" OR ") 
  }
  
  def AndPhrase(q: Query, f: Field, v: String) = Conjunction(q, FieldPhrase(f, v))

  val tripColPat = ".*\\.(arg1|arg2|rel|namespace)$"
  def OnTripleCols(t: Tuple): Tuple = Tuple(t.attrs.filterKeys(a => a.matches(tripColPat)))
  def ProjectTriples(ts: Tuples) = Operators.Project(OnTripleCols)(ts)
  
  type Tuples = Iterable[Tuple]
  type Attr = String
  type Search = Query => Tuples
  
  case class PartialSearcher(query: Query, search: Search)

  val Arg1Pat = "(.*)\\.arg1$".r
  val Arg2Pat = "(.*)\\.arg2$".r
  val RelPat = "(.*)\\.rel$".r
  def PartialSearchJoin(cond: BinaryPred) = {
    val leftAttr = cond.attr1
    val rightAttr = cond.attr2
    val (name, field) = rightAttr match {
      case Arg1Pat(n) => (n, arg1)
      case Arg2Pat(n) => (n, arg2)
      case RelPat(n) => (n, rel)
      case _ => throw new IllegalArgumentException("field must be arg1, rel, or arg2: " + rightAttr)
    }
    (ts: Tuples, ps: PartialSearcher) => {
      for (
          t1 <- ts.par;							// for each tuple
          v <- t1.getString(leftAttr).toSeq;		// get the value of its join attr
          q = AndPhrase(ps.query, field, v);// construct a complete query 
          t2 <- ps.search(q).par;
          t3 = t1.join(t2);					// join the tuples
          if cond(t3))						// test if the joined tuple satisfies the pred
        yield t3
    }.toList
  }
  
  
  val Arg1Eq = (v: String) => FieldPhrase(arg1_exact, v)
  val Arg2Eq = (v: String) => FieldPhrase(arg2_exact, v)
  val RelEq = (v: String) => FieldPhrase(rel_exact, v)
 
  val Arg1Cont = (v: String) => FieldKeywords(arg1, v)
  val Arg2Cont = (v: String) => FieldKeywords(arg2, v)
  val RelCont = (v: String) => FieldKeywords(rel, v)

  
}

object Tabulator {
  
  def trim(s: String, l: Int) = {
    val n = s.size
    s.substring(0, Math.min(l, n))
  }
  
  def valToString(v: Any) = v match {
    case x @ (_ :: _ :: _) => "{" + trim(x.mkString(", "), 40) + "}"
    case x @ (y :: _) => y.toString
    case x => x.toString
  }
  def tupleToList(t: Tuple, attrs: List[String]) = {
    for( 
    		a <- attrs;
    		v = t.attrs.getOrElse(a, "");
    		s = valToString(v)) yield s}.toList
    		
  def tuplesToTable(cols: List[String], ts: Iterable[Tuple]) = {
    val lst = ts.toList
    format(cols :: lst.map(t => tupleToList(t, cols)))
  }
  
  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
    rows.head ::
    rowSeparator ::
    rows.tail.toList :::
    rowSeparator ::
    List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString ("+", "+", "+")
}
