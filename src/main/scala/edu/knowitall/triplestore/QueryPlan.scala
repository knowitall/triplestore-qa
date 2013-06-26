package edu.knowitall.triplestore

import com.rockymadden.stringmetric.similarity._

case class Tuple(attrs: List[String], values: List[Any]) {
  if (attrs.size != values.size) {
    throw new IllegalArgumentException("illegal size: " + attrs + ", " + values)
  }
  val map = { attrs zip values }.toMap[String, Any]
  def get(attr: String): Option[Any] = {
    map.get(attr)
  }
  def getOrElse(attr: String, value: Any): Any = {
    map.getOrElse(attr, value)
  }
  def concatenate(other: Tuple): Tuple = {
    if (attrs.intersect(other.attrs).size == 0) {
      Tuple(attrs ++ other.attrs, values ++ other.values)
    } else {
      throw new IllegalArgumentException("tuple attrs not disjoint: " + List(attrs, other.attrs))
    }
  }
  def +(other: Tuple): Tuple = concatenate(other)
  def rename(newAttrs: List[String]): Tuple = Tuple(newAttrs, values)
  def renamePrefix(p: String): Tuple = rename(attrs map { a => p + "." + a })
  override def toString(): String = {
    val pairs = attrs zip values
    val strs = pairs map { case (a, b) => a + ": " + b}
    "(" + strs.mkString(", ") + ")"
  }
}

object Conditions {
  
  type JoinCond = (Tuple, Tuple) => Boolean
  
  type SelectCond = Tuple => Boolean

case class Contains[T](attr: String, value: T) extends SelectCond {
  def apply(t: Tuple): Boolean = {
    t.getOrElse(attr, List[T]()) match {
      case x: List[T] => x.contains(value)
      case _ => false
    }
  }
}

case class Intersects[T](attr1: String, attr2: String) extends JoinCond {
  def apply(t1: Tuple, t2: Tuple): Boolean = {
    val res1 = t1.getOrElse(attr1, List[T]())
    val res2 = t2.getOrElse(attr2, List[T]())
    (res1, res2) match {
      case (x1: List[T], x2: List[T]) => x1.intersect(x2).size > 0
      case _ => false
    }
  }
}

case class IntersectsSyns[T](attr1: String, attr2: String, syns: List[List[T]]) {
  def apply(t1: Tuple, t2: Tuple): Boolean = {
    val res1 = t1.getOrElse(attr1, List[T]())
    val res2 = t2.getOrElse(attr2, List[T]())
    (res1, res2) match {
      case (x1: List[T], x2: List[T]) => {
        syns.exists(syn => { !x1.intersect(syn).isEmpty && !x2.intersect(syn).isEmpty })
      }
      case _ => false
    }
  }
}

def sim(x: Any, y: Any): Double = {
  (x, y) match {
    case (x: String, y: String) => JaroWinklerMetric.compare(x, y).getOrElse(0.0)
    case _ => 0.0
  }
}


}

object Operators {
  
  
  case class Select(cond: Tuple => Boolean) {
    def apply(tuples: Iterable[Tuple]): Iterable[Tuple] = tuples.filter(cond)
  }
  
  case class Join(cond: (Tuple, Tuple) => Boolean) {
    def apply(tuples1: Iterable[Tuple], tuples2: Iterable[Tuple]): Iterable[Tuple] = {
      for (t1 <- tuples1; t2 <- tuples2; if cond(t1, t2)) yield t1 + t2
    }
  }
  
  case class Project(attrs: List[String]) {
    def proj(t: Tuple): Tuple = {
      Tuple(attrs, attrs.map(a => t.getOrElse(a, "")))
    }
    def apply(tuples: Iterable[Tuple]): Iterable[Tuple] = tuples.map(proj)
  }
  
}