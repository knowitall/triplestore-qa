package edu.knowitall.scoring.features

import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.Search.rel
import edu.knowitall.execution.Search.TSField
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution.SetTLiteral
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import TupleFeatures._

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

object QuerySimilarity extends AnswerGroupFeature("ExecQuery similarity") {

  import edu.knowitall.execution.StrSim.stops
  import edu.knowitall.execution.StrSim.lemmatize

  def lemmaFilter(lemma: Lemmatized[ChunkedToken]) = !lemma.token.isPunctuation && !lemma.string.isEmpty

  def toLemmaBag(str: String): Map[String, Int] = {
    val lemmas = lemmatize(str).filter(lemmaFilter).map(_.lemma)
    lemmas.groupBy(identity).map { case (lemma, lemmas) => (lemma, lemmas.size) }
  }

  def queryToBag(query: ConjunctiveQuery): Map[String, Int] = {
    // first, get all of the literals...
    val queryLiterals = query.conjuncts.flatMap(_.literalFields).map(_._2)
    // then get all of the strings
    def collectStrings(literal: TLiteral): Seq[String] = literal match {
      case UnquotedTLiteral(string) => Seq(string)
      case QuotedTLiteral(string) => Seq(string)
      case SetTLiteral(literals) => literals.flatMap(collectStrings)
    }
    val queryStrings = queryLiterals.flatMap(collectStrings)
    val lemmaCounts = queryStrings.flatMap(s => toLemmaBag(s).iterator)
    lemmaCounts.groupBy(_._1).map { case (lemma, lemmaCounts) => (lemma, lemmaCounts.map(_._2).sum) }
  }

  def simQ(originalQuery: ConjunctiveQuery, execQuery: ConjunctiveQuery): Double = {
    // turn query into bag-of-words
    val originalBag = queryToBag(originalQuery)
    val execBag     = queryToBag(execQuery)
    simCosine(originalBag, execBag)
  }

  def simCosine(a: Map[String, Int], b: Map[String, Int]): Double = {
    // norm
    val aNorm = math.sqrt(a.valuesIterator.map(v => v*v).sum)
    val bNorm = math.sqrt(b.valuesIterator.map(v => v*v).sum)
    // dot
    val sharedKeys = a.keySet.intersect(b.keySet).toSeq
    val dotVector = {
      for (key <- sharedKeys) yield {
        a(key) * b(key)
      }
    }
    val dotProduct = dotVector.sum.toDouble
    val cosineSim = dotProduct.toDouble / (aNorm * bNorm).toDouble
    cosineSim
  }

  def apply(group: AnswerGroup) = {
    val queryPairs = group.derivations.map(d => (d.parsedQuery, d.execTuple.query))
    val sims = queryPairs.map { case (oq, eq) => simQ(oq, eq) }
    val max = sims.max
    max
  }
}