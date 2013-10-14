package edu.knowitall.scoring.features

import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Tuple
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution.Tuple
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import TupleFeatures._
import edu.knowitall.execution.Search.TSField
import edu.knowitall.execution.Search.rel
import edu.knowitall.execution.TConjunct
import edu.knowitall.tool.stem.MorphaStemmer
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import edu.knowitall.execution.ConjunctiveQuery

case object LiteralFieldsDifference extends AnswerGroupFeature("Literal Fields Difference") {

  import FieldDifference.{conjuncts, avg}

  def apply(group: AnswerGroup) = {

    val queries = group.derivations.map(_.execTuple.query).distinct
    val cs = queries.flatMap(conjuncts).distinct

    val literalFields = cs.flatMap(_.literalFields).map(_._1.name).distinct
    require(literalFields.size >= 1, "No literals in any query conjunct.")
    val diffs = literalFields.map(f => FieldDifference(f).apply(group))
    val max = diffs.max
    max
  }
}

case class FieldDifference(field: String) extends AnswerGroupFeature("Query-Field similarity: " + field) {

  import FieldDifference._

  val splitRegex = "\\s+".r

  def queryLiteral(qconj: TConjunct) = {
    val desiredField = qconj.literalFields.find {
      case (f, v) => f.name == FieldDifference.this.field
    }
    desiredField.map {
      case (f, v) => v.toString
    }
  }

  def tupleLiteral(qconj: TConjunct, tuple: Tuple) = {
    val fullField = qconj.name + "." + FieldDifference.this.field
    tuple.get(fullField).map(_.toString)
  }

  def apply(group: AnswerGroup) = {

    val queriesToDerivations = group.derivations.groupBy(d => d.execTuple.query)
    val queriesToTuples = queriesToDerivations.map{case (query, derivs) => (query, derivs.map(d => d.execTuple.tuple)) }
    val conjunctsToTuples = queriesToTuples.flatMap { case (query, tuples) =>
      conjuncts(query).map { c => (c, tuples) }
    }
    val diffs = conjunctsToTuples.flatMap { case (conjunct, tuples) =>
      queryLiteral(conjunct) match {
        case Some(qs) => {
          val tliterals = tuples.map(t => tupleLiteral(conjunct, t).get).distinct
          tliterals.map { tl => cleanTokenDifference(qs, tl) }
        }
        case None => Seq(0)
      }
    }

    diffs.max
  }

  def cleanTokenDifference(literal1: String, literal2: String): Int = {

    val ls1 = splitRegex.split(literal1)
    val ls2 = splitRegex.split(literal2)

    val cls1 = normalizeLiteral(ls1).toSet
    val cls2 = normalizeLiteral(ls2).toSet

    symmetricDiff(cls1, cls2).size
  }

  def symmetricDiff[T](s1: Set[T], s2: Set[T]): Set[T] = {
    val leftDiff = s2.diff(s1)
    val rightDiff = s1.diff(s2)
    leftDiff.union(rightDiff)
  }
}

object FieldDifference {
  import TriplestoreFeatures.AnswerFrequency.junkTokens
  def avg(ns: Iterable[Double]) = ns.sum.toDouble / ns.size.toDouble

  def normalizeLiteral(ss: Seq[String]) = {
    val lcs = ss.map(_.toLowerCase)
    val stems = lcs.map(MorphaStemmer.apply)
    def tokenFilter(t: String) = !junkTokens.contains(t)
    val filtered = stems.filter(tokenFilter)
    filtered
  }

  def conjuncts(query: ConjunctiveQuery) = query.conjuncts
}