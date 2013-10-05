package edu.knowitall.paralex

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.wikianswers.QuestionCluster
import edu.knowitall.wikianswers.Question

case class TemplateCounter(cluster: QuestionCluster) {
  
  val genAbs = AbstractedQuestion.generateAbstracted _
 
  val templateStrings = cluster.questions.flatMap(q => genAbs(q.annotated).map(_.template.templateString)).distinct
  
  val valueStrings = cluster.questions.flatMap(q => genAbs(q.annotated).map(_.valueString)).distinct
  
  def templatePairs: Set[(String, String)] = {
    val pairs = cluster.questions.combinations(2).collect {
      case Seq(q1, q2) => (q1, q2)
    }
    pairs.flatMap(Function.tupled(getTemplateMatches)).toSet
  }
  
  def getTemplateMatches(q1: Question, q2: Question): Set[(String, String)] = {
    val pairs = for (a1 <- genAbs(q1.annotated);
         vs1 = a1.valueString;
    	 ts1 = a1.template.templateString;
    	 a2 <- genAbs(q2.annotated);
    	 vs2 = a2.valueString;
    	 ts2 = a2.template.templateString;
    	 if vs1 == vs2;
    	 if ts1 != ts2) yield (ts1, ts2)
    pairs.toSet
  }

}