package edu.knowitall.scoring

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.SetTLiteral
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized

/**
 *  Could be used to implement an "answer ranker"
 */
case class AnswerDerivationOrderingRanker() /* extends AnswerRanker */ {

  def rankAnswers(originalQuestion: String, groups: Seq[AnswerGroup]): Seq[ScoredAnswerGroup] = {

    // get a flat list of (derivation, group)
    val derivs = groups.flatMap(g => g.derivations.map(d => (g, d)))
    // sort according to the comparator...
    val sortedDerivs = derivs.sortBy(_._2)(AnswerDerivationOrdering).reverse
    val rearrangedDerivs = sortedDerivs.zipWithIndex.map { case ((group, deriv), rank) => (group, deriv, rank) }

    // get a map from group to derivation, rank.
    val groupMap = rearrangedDerivs.groupBy(_._1)

    // use index as the "score"
    val rankedGroups = groups.map { group =>
      val rank = groupMap(group).map(_._3).max
      BasicScoredAnswer(group.answer, group.alternates, group.derivations, rank)
    }
    rankedGroups.sortBy(-_.score)
  }
}

case object AnswerDerivationOrdering extends Ordering[AnswerDerivation] {

  val parseOrdering = ParseOrdering

  override def compare(a: AnswerDerivation, b: AnswerDerivation): Int = {

    // compute query ordering
    val qo = parseOrdering.compare(a, b)

    val asim = simQueryTuple(a.execTuple.query, a.execTuple.tuple)

    val bsim = simQueryTuple(b.execTuple.query, b.execTuple.tuple)

    if (qo != 0) qo else asim.compareTo(bsim)
  }

  def simQueryTuple(q: ConjunctiveQuery, tuple: Tuple): Double = {
    val queryBag = features.QuerySimilarity.queryToBag(q)
    // turn the literal fields of the tuple into a bag...
    // figure out which are the literal fields
    val literalFields = q.conjuncts.flatMap {
      case TConjunct(conjunctName, values) =>
        values.map { case (field, _) => conjunctName + "." + field.name }
    }
    // extract the string values from the tuple for each literal field
    val literalStrings = literalFields.flatMap(tuple.getString)
    val lemmaCounts = literalStrings.flatMap(ls => features.QuerySimilarity.toLemmaBag(ls).iterator).toSeq
    val tupleBag = lemmaCounts.groupBy(_._1).map { case (lemma, lemmaCounts) => (lemma, lemmaCounts.map(_._2).sum) }
    features.QuerySimilarity.simCosine(queryBag, tupleBag)
  }

  def symmetricDiffSize[T](set1: Set[T], set2: Set[T]): Int = {
    val notInSet2 = set1.diff(set2)
    val notInSet1 = set2.diff(set1)
    notInSet1.size + notInSet2.size
  }
}

case object ParseOrdering extends Ordering[AnswerDerivation] {

  import edu.knowitall.execution.StrSim.stops
  import edu.knowitall.execution.StrSim.lemmatize

  def lemmaFilter(lemma: Lemmatized[ChunkedToken]) = !lemma.token.isPunctuation && !lemma.string.isEmpty

  def toLemmaBag(str: String) = lemmatize(str).filter(lemmaFilter).map(_.string).filterNot(stops.contains).toSet

  // order any non-paraphrased parse ahead of any paraphrased parse
  // otherwise put the most specific parse first
  override def compare(a: AnswerDerivation, b: AnswerDerivation): Int = {
    // is it paraphrased?
    val ap = a.paraphrase.target != a.question
    val bp = b.paraphrase.target != b.question

    // similarity between question and query?
    def asim = -1.0 * features.QuerySimilarity.simQ(a.parsedQuery, a.execTuple.query)
    def bsim = -1.0 * features.QuerySimilarity.simQ(b.parsedQuery, b.execTuple.query)
    implicitly[Ordering[(Boolean, Double)]].compare((ap, asim), (bp, bsim))
  }
}