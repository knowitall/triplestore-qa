package edu.knowitall.scoring.training

import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.apps.QASystem
import edu.knowitall.tool.conf.Labelled

/**
 * For now, annotate training data with most common relations that accompany an answer.
 */
object AnalyzeTraining {

  def qaSystem =  QASystem.getInstance()
  
  def input = TrainingDataReader.defaultTraining 
  
  val articles = Set("a", "an", "the")
  
  def top[E](es: Seq[E], n: Int) = {
    val grouped = es.groupBy(identity)
    val counts = grouped.iterator.map(p => (p._1, p._2.size))
    counts.toSeq.sortBy(-_._2).map(_._1).take(n)
  }
  
  def output: Iterable[String] = input.map { labelled =>
    val group = labelled.item  
    val query = top(group.derivations.map(_.etuple.equery), 1).head
    val queryConjuncts = query match {
      case q: ExecConjunctiveQuery => q.conjuncts
      case _ => Nil
    }
    val conjunctNames = queryConjuncts.map(_.name)
    val queryString = queryConjuncts.mkString(", ")
    val answerderivs = group.derivations.iterator
    val tuples = answerderivs.map(_.etuple.tuple)
    val rels = conjunctNames.flatMap(name => tuples.flatMap(_.getString(name + ".rel")))
    val tokenizedRels = rels.map(rel => rel.toLowerCase.split(" "))
    val filteredRels = tokenizedRels.map(trel => trel.filter(t => !articles.contains(t)))
    val detokenizedFilteredRels = filteredRels.map(trel => trel.mkString(" "))
    val topRels = top(detokenizedFilteredRels.toSeq, 3)
    toOutputString(labelled, queryString, topRels)
  }
  
  def toOutputString(labelled: Labelled[AnswerGroup], queryString: String, topRels: Seq[String]): String = {
    val label = if (labelled.label) "1" else "0"
    val group = labelled.item
    val answer = group.alternates.head.head

    Seq(label, answer, topRels.mkString(","), queryString).mkString("\t")
  }
  
  def main(args: Array[String]): Unit = {
    
    import java.io.PrintStream
    
    val out = if (args.length >= 1) new PrintStream(args(0)) else System.out
    
    output foreach out.println
    
    if (out != System.out) out.close    
  }
}