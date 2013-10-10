package edu.knowitall.scoring

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.SetTLiteral
import edu.knowitall.execution.TLiteral
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized

case class AnswerDerivationOrderingRanker() extends AnswerRanker {

  override def rankAnswers(originalQuestion: String, groups: Seq[AnswerGroup]): Seq[ScoredAnswerGroup] = {

    val comparator = AnswerDerivationOrdering(originalQuestion)

    // get a flat list of (derivation, group)
    val derivs = groups.flatMap(g => g.derivations.map(d => (g, d)))
    // sort according to the comparator...
    val sortedDerivs = derivs.sortBy(_._2)(comparator).reverse
    val rearrangedDerivs = sortedDerivs.zipWithIndex.map { case ((group, deriv), rank) => (group, deriv, rank) }

    //rearrangedDerivs.foreach { case (g, d, r) => println(d.etuple.equery.uquery.question, d.etuple.equery.uquery, edu.knowitall.util.TuplePrinter.printTuple(d.etuple.tuple)) }

    // get a map from group to derivation, rank.
    val groupMap = rearrangedDerivs.groupBy(_._1)

    val rankedGroups = groups.map { group =>
      val rank = groupMap(group).map(_._3).max
      BasicScoredAnswer(group.answer, group.alternates, group.derivations, rank)
    }
    rankedGroups.sortBy(-_.score)
  }
}

case class AnswerDerivationOrdering(originalQuestion: String) extends Ordering[AnswerDerivation] {

  override def compare(a: AnswerDerivation, b: AnswerDerivation): Int = {

    // compute query ordering
    val qo = ParseOrdering.compare(a.etuple.equery.uquery, b.etuple.equery.uquery)

    val asim = a.etuple.equery match {
      case ExecConjunctiveQuery(conjQuery, _) => simQueryTuple(conjQuery, a.etuple.tuple)
      case _ => throw new RuntimeException("implementation error")
    }

    val bsim = b.etuple.equery match {
      case ExecConjunctiveQuery(conjQuery, _) => simQueryTuple(conjQuery, b.etuple.tuple)
      case _ => throw new RuntimeException("implementation error")
    }

    if (qo != 0) qo else asim.compareTo(bsim)
  }

  def simQueryTuple(q: ConjunctiveQuery, tuple: Tuple): Double = {
    val queryBag = ParseOrdering.queryToBag(q)
    // turn the literal fields of the tuple into a bag...
    // figure out which are the literal fields
    val literalFields = q.conjuncts.flatMap { case TConjunct(conjunctName, values) =>
      values.map { case (field, _) => conjunctName + "." + field.name }
    }
    // extract the string values from the tuple for each literal field
    val tupleBag = literalFields.flatMap(tuple.getString).flatMap(ParseOrdering.toLemmaBag).toSet
    symmetricDiffSize(queryBag, tupleBag)
  }

  def symmetricDiffSize[T](set1: Set[T], set2: Set[T]): Int = {
    val notInSet2 = set1.diff(set2)
    val notInSet1 = set2.diff(set1)
    notInSet1.size + notInSet2.size
  }

  // compare the parse: we need to add some way to know what the original question was.
  // The plan for that was to add a field to.... what? I think it makes the most sense to add it
  // to the uquery.... maybe uquery should have two fields, the question and the execquestion, or something like that.
  // Or tony's suggestion, which was to add it as a field to AnswerDerivation, or to AnswerGroup? If we add it here later down the
  // line, then we wouldn't be able to rank actual answerderivations. In order for this to actually be an ordering over answerderivations,
  // it has to be at latest part of the AnswerDerivation, or the query.
  object ParseOrdering extends Ordering[UQuery] {

    import edu.knowitall.execution.StrSim.stops
    import edu.knowitall.execution.StrSim.lemmatize

    def lemmaFilter(lemma: Lemmatized[ChunkedToken]) = !lemma.token.isPunctuation && !lemma.string.isEmpty

    def toLemmaBag(str: String) = lemmatize(str).filter(lemmaFilter).map(_.string).filterNot(stops.contains).toSet

    val questionBag: Set[String] = toLemmaBag(originalQuestion)

    override def compare(a: UQuery, b: UQuery): Int = {
      // is it paraphrased?
      val ap = a.question != originalQuestion
      val bp = b.question != originalQuestion

      // similarity between question and query?
      val asim = sim(a)
      val bsim = sim(b)
      implicitly[Ordering[(Boolean, Double)]].compare((ap, asim), (bp, bsim))
    }

    def sim(uquery: UQuery): Double = {
      uquery match {
        case q: ConjunctiveQuery => simQ(q)
        case _ => throw new RuntimeException("Implementation error.")
      }
    }

    def queryToBag(query: ConjunctiveQuery): Set[String] = {
      // first, get all of the literals...
      val queryLiterals = query.conjuncts.flatMap(_.literalFields).map(_._2)
      // then get all of the strings
      def collectStrings(literal: TLiteral): Seq[String] = literal match {
        case UnquotedTLiteral(string) => Seq(string)
        case QuotedTLiteral(string) => Seq(string)
        case SetTLiteral(literals) => literals.flatMap(collectStrings)
      }
      val queryStrings = queryLiterals.flatMap(collectStrings)
      queryStrings.flatMap(toLemmaBag).toSet
    }

    def simQ(query: ConjunctiveQuery): Double = {
      // turn query into bag-of-words
      val queryBag = queryToBag(query)
      // compute the size of the symmetric difference...
      symmetricDiffSize(queryBag, questionBag)
    }
  }
}