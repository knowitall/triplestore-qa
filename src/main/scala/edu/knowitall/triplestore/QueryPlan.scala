package edu.knowitall.triplestore

import com.rockymadden.stringmetric.similarity._


case class Tuple(fields: Map[String, List[String]]) {
    
  def join(other: Tuple): Tuple = {
    val t = Tuple(this.fields ++ other.fields)
    if (t.fields.size == this.fields.size + other.fields.size) {
      return t
    } else {
      throw new IllegalArgumentException("field names not disjoint: " + this + ", " + other)
    }
  }
  
  def rename(f: String => String): Tuple = {
    Tuple(fields.map{ case (k, v) => (f(k), v) })
  }
  
  def renamePrefix(p: String): Tuple = rename(k => p + "." + k)
  
  override def toString: String = {
    val pairs = fields map { case (k, v) => k + ": " + v }
    return "(" + pairs.mkString(", ") + ")"
  }
}

object Conditions {
  

  type Attr = String
  type Value = List[String]
  type BinaryStringPred = (String, String) => Boolean
  type ValuePred = Value => Boolean
  type BinaryValuePred = (Value, Value) => Boolean
  type TuplePred = Tuple => Boolean

  def allPairs(v1: Value, v2: Value) = for (x <- v1; y <- v2) yield (x, y)
  
  def somePairPred(f: BinaryStringPred): BinaryValuePred =
    (v1: Value, v2: Value) => allPairs(v1, v2).exists(f.tupled)
  
  def allPairPred(f: BinaryStringPred): BinaryValuePred = 
    (v1: Value, v2: Value) => allPairs(v1, v2).forall(f.tupled)
    
  def valPair(a1: Attr, a2: Attr, t: Tuple) = for (v1 <- t.fields.get(a1); v2 <- t.fields.get(a2)) yield (v1, v2)
  
  def binaryAttrPred(a1: Attr, a2: Attr, f: BinaryValuePred): TuplePred = (t: Tuple) => {
    valPair(a1, a2, t).exists(f.tupled)
  }
  
  def unaryAttrPred(a: Attr, x: String, f: BinaryValuePred): TuplePred = (t: Tuple) => {
    { for (v <- t.fields.get(a)) yield (v, List(x)) }.exists(f.tupled)
  }
  
  def binaryPredFromString(a1: Attr, a2: Attr, f: BinaryStringPred) = binaryAttrPred(a1, a2, somePairPred(f))
  
  def unaryPredFromString(a: Attr, v: String, f: BinaryStringPred) = unaryAttrPred(a, v, somePairPred(f)) 

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
  def strSim(thresh: Double) = 
    (x: String, y: String) => { JaroWinklerMetric.compare(x, y).exists(_ > thresh) }

    
  def AttrsEqual(a1: Attr, a2: Attr): TuplePred = binaryPredFromString(a1, a2, strEq)
  def AttrEquals(a: Attr, v: String): TuplePred = unaryPredFromString(a, v, strEq)
  
  def AttrsSim(a1: Attr, a2: Attr, t: Double): TuplePred = binaryPredFromString(a1, a2, strSim(t))
  def AttrSim(a: Attr, v: String, t: Double): TuplePred = unaryPredFromString(a, v, strSim(t))
  
  def AttrsSyn(a1: Attr, a2: Attr, s: Synonyms): TuplePred = binaryPredFromString(a1, a2, inSameSet(s))

  def On(attrs: Attr*) = (t: Tuple) => {
    val items = for (a <- attrs; v <- t.fields.get(a)) yield (a, v)
    Tuple(items.toMap)
  }

}

object Operators {
  type TuplePred = Tuple => Boolean
  type TupleMap = Tuple => Tuple
  type Tuples = Iterable[Tuple]
  
  def Select(p: TuplePred) = (ts: Tuples) => ts.filter(p)
  
  def Project(m: TupleMap) = (ts: Tuples) => ts.map(m)
  
  def NestedLoopJoin(p: TuplePred) = (ts1: Tuples, ts2: Tuples) => {
    for (t1 <- ts1; t2 <- ts2; j = t1.join(t2); if p(j)) yield j
  }
  
}

object Search {
  
  object Field extends Enumeration {
    type Field = Value
    val arg1, rel, arg2, namespace = Value
  }
  import Field._
  
  trait Query {
    def toQueryString: String
  }
  
  case class FieldEquals(f: Field, v: String) extends Query {
    def toQueryString = f.toString() + ":\"" + v + "\""
  }
  
  case class Conjunction(conjuncts: Query*) extends Query {
    def toQueryString = conjuncts.map("(" + _.toQueryString + ")").mkString(" AND ") 
  }

  case class Disjunction(disjuncts: Query*) extends Query {
    def toQueryString = disjuncts.map("(" + _.toQueryString + ")").mkString(" OR ") 
  }
  
  def FieldIn(f: Field, vs: Seq[String]) = Disjunction(vs.map(v => FieldEquals(f, v)):_*)
  
}
