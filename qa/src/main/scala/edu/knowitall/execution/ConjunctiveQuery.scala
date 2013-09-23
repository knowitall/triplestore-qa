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
trait TLiteral extends TVal {
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
  def fromStringMult(s: String): List[TVariable] = {
    val parts = s.split(", *").toList
    for (p <- parts; t = TVariable.fromString(p)) yield t match {
      case Some(x) => x
      case None => throw new 
        IllegalArgumentException(s"Could not parse variables in: $s")
    }
  }
}

/**
 * Triple values can also be the disjunction of a set of literals. 
 */
case class SetTLiteral(values: List[TLiteral]) extends TVal with TLiteral {
  override def toString = values.mkString(" | ") 
  override def toConjunct(field: Field) =
    Disjunction(values.map(_.toConjunct(field)):_*)
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
      case l: TLiteral => Some((f, l))
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
  
  def getTLiteral(s: String): TLiteral = {
    val pieces = s.split("\\|").toList.map(_.trim())
    pieces match {
      case x :: Nil => QuotedTLiteral.fromString(x) match {
        case Some(QuotedTLiteral(y)) => QuotedTLiteral(y)
        case _ => UnquotedTLiteral(s)
      }
      case _ => SetTLiteral(pieces.map(getTLiteral(_)))
    }
  }
  
  def getTVal(s: String): TVal = {
    val v = TVariable.fromString(s)
    v match {
      case Some(TVariable(x)) => TVariable(x)
      case _ => getTLiteral(s)
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
  
  def replaceField(c: TConjunct, f: Field, vs: List[TVal]): List[TConjunct] = {
    for (v <- vs) yield TConjunct(c.name, c.values + (f -> v))
  }
  
  def expandSetTLiteralsField(c: TConjunct, f: Field): List[TConjunct] = {
    c.values.get(f) match {
      case Some(v: SetTLiteral) => replaceField(c, f, v.values)
      case _ => List(c)
    }
  }
  
  def getSetTLiteral(c: TConjunct): Option[Field] = c.values.keys.find(c.values(_).isInstanceOf[SetTLiteral])
  
  def expandSetTLiterals(c: TConjunct): List[TConjunct] = {
    getSetTLiteral(c) match {
      case Some(f: Field) => expandSetTLiteralsField(c, f).flatMap(expandSetTLiterals)
      case _ => List(c)
    }
  }
} 

/**
 * A conjunctive query consists of a list of qvars (query variables) and a list 
 * of conjuncts. A conjunctive query represents a select-join-project type
 * operation. The list of conjuncts represents the data to be selected.
 * The shared variables among the conjuncts encodes the join predicates. 
 * The qvar encodes the projection variable. qAttr is the tuple-attribute
 * to project onto. 
 */
trait ConjunctiveQuery extends UQuery {
  val qVars: List[TVariable]
  val qAttrs: List[String]
  val conjuncts: List[TConjunct]
  
  override def toString(): String = {
    val varString = qVars.map(_.toString).mkString(",")
    val conjString = conjuncts.mkString(" ")
    varString + ": " + conjString 
  }
}

/**
 * A conjunctive query backed by a list of conjuncts.
 */
case class ListConjunctiveQuery(question: String, qVars: List[TVariable], 
    conjuncts: List[TConjunct])
  extends ConjunctiveQuery {
  
  val conjunctNames = conjuncts.map(_.name)
  if (conjunctNames.distinct.size != conjunctNames.size) throw new 
    IllegalArgumentException(s"Conjuncts must have distinct names: $conjuncts")
  
  val qas = {for (v <- qVars; c <- conjuncts; a <- c.attrName(v)) yield (v, a)}.groupBy(_._1)
  val qAttrs = for (v <- qVars; group <- qas.get(v); (v, a) <- group.find(x => true)) yield a
  
}
case object ListConjunctiveQuery {
  def fromString(s: String): Option[ListConjunctiveQuery] = {
    val parts = s.split(":", 2)
    if (parts.size == 2) {
      val left = parts(0)
      val qVars = TVariable.fromStringMult(parts(0)) match {
        case head :: rest => head :: rest
        case _ => 
          throw new IllegalArgumentException(s"Expected variable: $left")
      }
      val conjuncts = TConjunct.fromStringMult(parts(1))
      Some(ListConjunctiveQuery(s, qVars, conjuncts.toList))
    } else if (parts.size == 1) {
      val s = parts(0)
      val conjuncts = TConjunct.fromStringMult(s)
      val qVars = conjuncts.flatMap(_.vars).toList match {
        case v :: rest => v :: rest
        case _ => throw new IllegalArgumentException(s"Expected variable: $s")
      }
      Some(ListConjunctiveQuery(s, qVars.distinct, conjuncts.toList))
    } else {
      None
    }
  }


  def expandSetTLiterals(cq: ConjunctiveQuery): List[ListConjunctiveQuery] = {
    val css = for (c <- cq.conjuncts; cs = TConjunct.expandSetTLiterals(c)) yield cs
    val product = Utils.cartesian[TConjunct](css).toList
    for (cs <- product) yield ListConjunctiveQuery(cq.question, cq.qVars, cs.toList)
  }

}

/**
 * A simple query is a conjunctive query that has a single conjunct.
 */
case class SimpleQuery(question: String, name: String, map: Map[Field, TVal])
  extends ConjunctiveQuery { 
  val conjunct = TConjunct(name, map)
  val conjuncts = List(conjunct)
  val vars = map.values.collect{ case x: TVariable => x }.toList
  val qVars = vars match {
    case v :: Nil => List(v)
    case _ => throw new 
      IllegalArgumentException(s"SimpleQuery must have exactly one variable, "
          + s"got: $vars")
  }
  val qAttrs = List(conjunct.joinKeys(qVars(0)))
}
case object SimpleQuery {
  def fromString(s: String) = TConjunct.fromString("r", s) match {
    case Some(TConjunct(name, map)) => Some(SimpleQuery(s, name, map))
    case _ => None
  }
}

object Utils {
  def cartesian[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])) {
    (x, y) => for (a <- x.view; b <- y) yield a :+ b
  }
}
