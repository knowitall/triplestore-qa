package edu.knowitall.execution

import Search.Field
import Search.TSQuery
import Search.{FieldKeywords, FieldPhrase}
import Search.Conjunction
import org.slf4j.LoggerFactory
import Search._
import scala.Option.option2Iterable

/**
 * Base trait for triple values.
 */
trait TVal

/**
 * Base trait for triple literal values.
 */
trait TLiteral {
  val value: String
  def toConjunct(field: Field): TSQuery
}

/**
 * Triple literals can be unquoted, which has the semantics of doing a keyword 
 * search over a field.
 */
case class UnquotedTLiteral(value: String) extends TVal with TLiteral {
  override def toString = value
  override def toConjunct(field: Field) = FieldKeywords(field, value)
}

/**
 * Triple literals can also be quoted, which has the semantics of doing an
 * exact-match search over a field.
 */
case class QuotedTLiteral(value: String) extends TVal with TLiteral {
  override def toString = s""""$value""""
  override def toConjunct(field: Field) = FieldPhrase(field, value)
}
case object QuotedTLiteral {
  val quoted = """^"(.*)"$""".r
  def fromString(s: String): Option[QuotedTLiteral] = s.trim() match {
    case quoted(s) => Some(QuotedTLiteral(s))
    case _ => None
  }
}

/**
 * Triple values can also be variables, which have a string name.
 */
case class TVariable(name: String) extends TVal {
  override def toString = "$" + name
}
case object TVariable {
  val vpat = """\$(.*)""".r
  def fromString(s: String): Option[TVariable] = s.trim() match {
    case vpat(v) => Some(TVariable(v))
    case "?" => Some(TVariable("?"))
    case _ => None
  }
}

/**
 * TConjunct objects have a name (you can think of this as a unique
 * identifier for the relational table returned) and map containing the
 * field values. The field values map a field (arg1, rel, or arg2) to a 
 * value (literal or variable).
 */
case class TConjunct(name: String, values: Map[Field, TVal]) {  
  
  def literalFields: Iterable[(Field, TLiteral)] = { 
    for ((f, v) <- values) yield v match {
      case l: UnquotedTLiteral => Some((f, l))
      case l: QuotedTLiteral => Some((f, l))
      case _ => None
    }
  }.flatten
  
  def variableFields: Iterable[(Field, TVariable)] = {
    for ((f, v) <- values) yield v match {
      case TVariable(s) => Some((f, TVariable(s)))
      case _ => None
    }
  }.flatten
  
  def varsToFields: Map[TVariable, Field] = variableFields.map(_.swap).toMap
  
  def partialQuery: TSQuery = {
    val lfs = literalFields.toList
    val conjuncts = for ((f, v) <- lfs) yield v.toConjunct(f)
    Conjunction(conjuncts.toList:_*)
  }
  
  def joinKeys: Map[TVariable, String] = {
    val vfs: Map[TVariable, Field] = varsToFields
    val pairs = for (v <- vfs.keys; f <- vfs.get(v); a = name + "." + f)
      yield (v, a)
    pairs.toMap
  }
  
  def attrName(v: TVariable): Option[String] = joinKeys.get(v)
  
  def vars: Iterable[TVariable] = varsToFields.keys.toSet
  
  val xs = values.getOrElse(arg1, "")
  val rs = values.getOrElse(rel, "")
  val ys = values.getOrElse(arg2, "")
  override def toString = s"($xs, $rs, $ys)"
}
case object TConjunct {
  
  val logger = LoggerFactory.getLogger(this.getClass) 
  
  val qpat = """\(?(.+),(.+),(.+?)\)?""".r

  def fromString(name: String, s: String): Option[TConjunct] = s match {
    case qpat(x, r, y) => Some(fromTriple(name, x, r, y))
    case _ => None
  }
  
  def getTVal(s: String): TVal = {
    val v = TVariable.fromString(s)
    val q = QuotedTLiteral.fromString(s)
    v match {
      case Some(TVariable(x)) => TVariable(x)
      case _ => q match {
        case Some(QuotedTLiteral(x)) => QuotedTLiteral(x)
        case _ => UnquotedTLiteral(s) 
      }
    }
  }
  
  val fields = List(arg1, rel, arg2)
  def fromTriple(name: String, x: String, r: String, y: String): TConjunct = {
    val lst = List(x.trim(), r.trim(), y.trim())
    val items = for ((f, a) <- fields.zip(lst); v = getTVal(a)) yield (f, v)
    TConjunct(name, items.toMap)
  }
  
  val splitPat = """(?<=\))\s*?(?=\()"""
  def fromStringMult(s: String): Iterable[TConjunct] = {
    val parts = s.split(splitPat).toList.map(_.trim).filterNot(_ == "")
    val queries = { for ((s, i) <- parts.zipWithIndex; 
                       q <- fromString(s"r$i", s)) yield q }.toList
    queries
  }
} 

/**
 * A conjunctive query consists of a qvar (query variable) and a list of 
 * conjuncts. A conjunctive query represents a select-join-project type
 * operation. The list of conjuncts represents the data to be selected.
 * The shared variables among the conjuncts encodes the join predicates. 
 * The qvar encodes the projection variable. qAttr is the tuple-attribute
 * to project onto. 
 */
trait ConjunctiveQuery extends UQuery {
  val qVar: TVariable
  val qAttr: String
  val conjuncts: List[TConjunct]
}

/**
 * A conjunctive query backed by a list of conjuncts.
 */
case class ListConjunctiveQuery(qVar: TVariable, conjuncts: List[TConjunct])
  extends ConjunctiveQuery {
  
  val conjunctNames = conjuncts.map(_.name)
  if (conjunctNames.distinct.size != conjunctNames.size) throw new 
    IllegalArgumentException(s"Conjuncts must have distinct names: $conjuncts")
  
  val qas = {for (c <- conjuncts; a <- c.attrName(qVar)) yield a}.toList
  val qAttr = qas match {
    case a :: rest => a
    case _ => throw new IllegalArgumentException(s"Query variable $qVar must "
        + s"appear in at least one conjunct in $conjuncts")
  }
}
case object ListConjunctiveQuery {
  def fromString(s: String): Option[ConjunctiveQuery] = {
    val parts = s.split(":", 2)
    if (parts.size == 2) {
      val left = parts(0)
      val qVar = TVariable.fromString(parts(0)) match {
        case Some(TVariable(v)) => TVariable(v)
        case _ => throw new IllegalArgumentException(s"Expected variable: $left")
      }
      val conjuncts = TConjunct.fromStringMult(parts(1))
      Some(ListConjunctiveQuery(qVar, conjuncts.toList))
    } else if (parts.size == 1) {
      val s = parts(0)
      val conjuncts = TConjunct.fromStringMult(s)
      val qVar = conjuncts.flatMap(_.vars).toList match {
        case v :: rest => v
        case _ => throw new IllegalArgumentException(s"Expected variable: $s")
      }
      Some(ListConjunctiveQuery(qVar, conjuncts.toList))
    } else {
      None
    }
  } 
}

/**
 * A simple query is a conjunctive query that has a single conjunct.
 */
case class SimpleQuery(name: String, map: Map[Field, TVal])
  extends ConjunctiveQuery { 
  val conjunct = TConjunct(name, map)
  val conjuncts = List(conjunct)
  val vars = map.values.collect{ case x: TVariable => x }.toList
  val qVar = vars match {
    case v :: Nil => v
    case _ => throw new 
      IllegalArgumentException(s"SimpleQuery must have exactly one variable, "
          + s"got: $vars")
  }
  val qAttr = conjunct.joinKeys(qVar)
}
case object SimpleQuery {
  def fromString(s: String) = TConjunct.fromString("r", s) match {
    case Some(TConjunct(name, map)) => Some(SimpleQuery(name, map))
    case _ => None
  }
}