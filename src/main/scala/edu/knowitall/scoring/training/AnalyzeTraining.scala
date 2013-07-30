package edu.knowitall.scoring.training

import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.Search.TSField
import edu.knowitall.execution.Search
import edu.knowitall.execution.ExecQuery
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.scoring.LogisticAnswerScorer
import edu.knowitall.apps.QASystem
import edu.knowitall.tool.conf.Labelled

/**
 * For now, annotate training data with most common relations that accompany an answer.
 */
object AnalyzeTraining {
  
  def scorer = LogisticAnswerScorer()
  
  val featureSet = scorer.featureSet
  
  def input = TrainingDataReader.defaultTraining 
  
  val articles = Set("a", "an", "the")
  
  def topN[E](es: Seq[E], n: Int) = {
    val grouped = es.groupBy(identity)
    val counts = grouped.iterator.map(p => (p._1, p._2.size))
    counts.toSeq.sortBy(-_._2).map(_._1).take(n)
  }
  
  def getConjuncts(query: ExecQuery): Seq[TConjunct] = {
    query match {
      case q: ExecConjunctiveQuery => q.conjuncts
      case _ => Nil
    }
  }
  
  def getFilteredFieldValues(ds: Seq[AnswerDerivation], f: TSField): Seq[String] = {
    val query = topN(ds.map(_.etuple.equery), 1).head
    val tuples = ds.map(_.etuple.tuple)
    val queryConjuncts = getConjuncts(query)
    val conjunctNames = queryConjuncts.map(_.name)
    val rels = conjunctNames.flatMap(name => tuples.flatMap(_.getString(s"$name.${f.name}")))
    val tokenizedRels = rels.map(rel => rel.toLowerCase.split(" "))
    val filteredRels = tokenizedRels.map(trel => trel.filter(t => !articles.contains(t)))
    val detokenizedFilteredRels = filteredRels.map(trel => trel.mkString(" "))
    detokenizedFilteredRels.toSeq
  } 
  
  def getTopFieldValues(group: AnswerGroup, f: TSField, n: Int) = {
    val ds = group.derivations
    topN(getFilteredFieldValues(ds, f), n)
  }
  
  def queryString(query: ExecQuery): String = {
    val queryConjuncts = getConjuncts(query)
    val conjunctNames = queryConjuncts.map(_.name)
    queryConjuncts.mkString(", ")
  }
  
  def mostCommonQuery(group: AnswerGroup) = topN(group.derivations.map(_.etuple.equery), 1).head
  
  def output: Iterable[String] = input map toOutputString

  
  def toOutputString(labelled: Labelled[AnswerGroup]): String = {
    val label = if (labelled.label) "1" else "0"
    val group = labelled.item
    val derivs  = group.derivations
    val query = mostCommonQuery(group)
    val answer = group.alternates.head.head
    val topArg1s = getTopFieldValues(group, Search.arg1, 1)
    val topRels  = getTopFieldValues(group, Search.rel, 1)
    val topArg2s  = getTopFieldValues(group, Search.arg2, 1)
    val numDerivs = group.derivations.size
    val conf = scorer.scoreAnswer(group).score
    val fields = Seq(label, 
        answer, 
        queryString(query),
        numDerivs,
        conf,
        topArg1s.mkString(","),
        topRels.mkString(","),
        topArg2s.mkString(",")
      )
    val featureFields = featureVector(labelled)
    (fields ++ featureFields).mkString("\t")
  }
  
  def featureVector(labelled: Labelled[AnswerGroup]) = featureSet.vectorize(labelled.item).map(_.toString)
  
  def headers = Seq(
      "label",
      "answer",
      "queryString",
      "numDerivs",
      "conf",
      "topArg1s",
      "topRels",
      "topArg2s"
  ) ++ featureSet.featureNames
  
  def headerString = headers.mkString("\t")
  
  def main(args: Array[String]): Unit = {
    
    import java.io.PrintStream
    
    val out = if (args.length >= 1) new PrintStream(args(0)) else System.out
    val outputList = output.toList
    out.println(headerString)
    outputList foreach out.println
    
    if (out != System.out) out.close    
  }
}