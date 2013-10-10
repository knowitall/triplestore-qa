package edu.knowitall.paralex

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.wikianswers.QuestionCluster
import edu.knowitall.wikianswers.Question

case class TemplateCounter(cluster: QuestionCluster) {
  
  val genAbs = AbstractedQuestion.generateAbstracted _
  
  val absClusters = cluster.questions.map(q => genAbs(q.annotated).take(10).toList)
 
  val templateStrings = absClusters.flatMap(cl => cl.map(_.template.templateString)).distinct
  
  val valueStrings = absClusters.flatMap(cl => cl.map(_.valueString)).distinct
  
  def templatePairs: Set[(String, String)] = {
    val pairs = absClusters.combinations(2).collect {
      case Seq(q1, q2) => (q1, q2)
    }
    pairs.flatMap(Function.tupled(getTemplateMatches)).toSet
  }
  
  def getTemplateMatches(cl1: List[AbstractedQuestion], cl2: List[AbstractedQuestion]): Set[(String, String)] = {
    val pairs = for (a1 <- cl1;
         vs1 = a1.valueString;
    	 ts1 = a1.template.templateString;
    	 a2 <- cl2;
    	 vs2 = a2.valueString;
    	 ts2 = a2.template.templateString;
    	 if vs1 == vs2;
    	 if ts1 != ts2) yield (ts1, ts2)
    pairs.toSet
  }

}