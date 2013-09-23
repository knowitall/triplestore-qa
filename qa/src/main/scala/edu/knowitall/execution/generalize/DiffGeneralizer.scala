package edu.knowitall.execution
package generalize

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery
import edu.knowitall.execution.StrSim
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.Search.Field

class DiffGeneralizingExecutor(val baseExecutor: QueryExecutor, diffGen: DiffGeneralizer, maxRewrites: Int) extends GeneralizingExecutor {
  

  
  def this(baseExec: QueryExecutor, minScore: Double, maxRewrites: Int) = 
    this(baseExec, new DiffGeneralizer("http://rv-n12.cs.washington.edu:10893/solr/triplestore", minScore), maxRewrites)
  
  /**
   * Supplies "reasonable" default values for minscore and maxrewrites.  
   */
  def this(baseExec: QueryExecutor) = this(baseExec, minScore = 1.5, maxRewrites = 5)
    
  import DiffGeneralizer.normTokens
  import DiffGeneralizer.Lemma
  
  case class DiffRewrite(conjName: String, field: Field, rewriteTokens: Seq[Lemma], score: Double) {
    def rewriteString = rewriteTokens.map(_.string).mkString(" ")
  }
  
  def generalizations(query: ListConjunctiveQuery): Iterator[ListConjunctiveQuery] = {
    
    // get a list of all the transformations we'd want to make to the query...
    // in the form of (conjunct name, field name, rewrite, score)
    // combine them and sort for all conjuncts in the query
    // then apply them one at a time for successively generalized queries.
    val rewrites = 
      for (c <- query.conjuncts;
           (field, literal) <- c.values.collect { case (field, UnquotedTLiteral(value)) => (field, value) };
           norm = normTokens(literal);
           (rewrite, score) <- diffGen.getGeneralizations(norm)) yield DiffRewrite(c.name, field, rewrite, score)
                   
    val sortedRewrites = rewrites.sortBy(-_.score)
    var appliedRewrites = List.empty[ListConjunctiveQuery]
    var lastQuery = query
    for (rewrite <- sortedRewrites) {
      val conjToRewrite = lastQuery.conjuncts.find(_.name == rewrite.conjName).get
      val rewrittenValues = conjToRewrite.values + (rewrite.field -> UnquotedTLiteral(rewrite.rewriteString))
      val rewrittenConj = conjToRewrite.copy(values = rewrittenValues)
      val rewrittenConjuncts = lastQuery.conjuncts.map { c => 
        if (c.name == rewrite.conjName) rewrittenConj else c  
      }
      val rewrittenQuery = lastQuery.copy(conjuncts = rewrittenConjuncts)
      appliedRewrites ::= rewrittenQuery
      lastQuery = rewrittenQuery
    }
    appliedRewrites.reverse.take(maxRewrites).iterator
  }
  
}

class DiffGeneralizer(val solrClient: SolrServer, minScore: Double) {
  
  import DiffGeneralizer._
  
  def this(url: String, minScore: Double) = this(new HttpSolrServer(url), minScore)
  
  /**
   * Get the template diff score for normTokens
   */
  def getDiffScore(lemmas: Seq[Lemma]): Option[Double] = {
    
    val norm = lemmas.map(_.lemma).mkString(" ")
    val queryString = "+arg1_exact:\"%s\" +rel_exact:template_diff_score".format(norm)
    val query = new SolrQuery(queryString)
    val result = solrClient.query(query)
    val docList = result.getResults
    val topDoc = if (docList.size > 0) Some(docList.get(0)) else None
    val scoreField = topDoc.map(_.getFieldValue("arg2_exact"))
    val score = scoreField.collect { case x: String => x.toDouble }
    score
  }
  
  /**
   * For all token subsequences in str, find diff score
   * and return as a sorted list with slice interval.
   */
  def allDiffScores(lemmas: Seq[Lemma]) = {
    val substrings = allSubseqs(lemmas)
    val scored = substrings.map { s => (s, getDiffScore(lemmas.slice(s.start, s.end)).getOrElse(-1.0)) }
    scored.sortBy(-_._2).filter(_._2 >= minScore)
  }
  
  /**
   * Get all diffs to drop in the order we want to drop them:
   * stop before we'd be left with an empty set of lemmas,i
   * filter out any overlaps
   */
  def getOrderedDropIntervals(lemmas: Seq[Lemma]): Seq[(Interval, Double)] = {
    // get all the intervals
    val allIntervals = allDiffScores(lemmas)
    val allIntervalsIter = allIntervals.iterator
    // keep a running list of the intervals we will drop...
    var dropSet = List.empty[Interval]
    // and a way to know when we would have dropped everything
    val allIndices = lemmas.indices.toSet
    def allDropped = dropSet.flatten.toSet == allIndices
    
    var resultIntervals = List.empty[(Interval, Double)]
    var done = false
    while (!done && allIntervalsIter.hasNext) {
      val (nextInterval, score) = allIntervalsIter.next
      if (!dropSet.exists(_.intersects(nextInterval))) {
        dropSet = nextInterval :: dropSet
        if (!allDropped) {
          resultIntervals = (nextInterval, score) :: resultIntervals
        } else {
          done = true
        }
      }
    }
    // reverse because prepending reversed their order
    resultIntervals.reverse
  }
  
  /**
   * Use the result of getOrderedDropIntervals to drop the specified intervals
   * from lemmas. 
   */
  def getGeneralizations(lemmas: Seq[Lemma]): Seq[(Seq[Lemma], Double)] = {
    val allDropIntervals = getOrderedDropIntervals(lemmas)
    var previouslyDropped = Set.empty[Int]
    allDropIntervals.map { case (interval, score) =>
      previouslyDropped ++= interval.toSet
      val dropped = lemmas.zipWithIndex.filter { case (lemma, index) => !previouslyDropped.contains(index) }  
      (dropped.map(_._1), score)
    }
  }
}

object DiffGeneralizer {
  
  type Lemma = Lemmatized[ChunkedToken] 
  
  def normTokens(str: String): Seq[Lemma] = StrSim.lemmatize(str)
  
  def allSubseqs[T](seq: Seq[T]): Seq[Interval] = {
    val indices = seq.indices
    for (i <- indices;
         j <- indices.drop(i)) yield Interval.open(i, j+1)
    
  }
}

object Test {
  
  import DiffGeneralizer._
  
  def main(args: Array[String]): Unit = {
    
    val dg = new DiffGeneralizingExecutor(null, new DiffGeneralizer("http://rv-n12:10893/solr/triplestore", 0.0), 4)
    
    //val qs = "$x: ($x, is a great example of, a common cat species) ($x, is the other name for, a business organization)"
    val qs = "$x: ($x, is the most commonly used car in, the US)"
    
    val query = ListConjunctiveQuery.fromString(qs).get
    
    dg.generalizations(query) foreach { q =>
      println(q)
    }
  }  
}