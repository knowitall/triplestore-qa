package edu.knowitall.paralex

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.wikianswers.QuestionCluster

case class TemplateCounter(cluster: QuestionCluster) {
  
  val absCluster = cluster.questions.flatMap(q => AbstractedQuestion.generateAbstracted(q.annotated)).distinct
  
  val templateStrings = absCluster.map(abs => abs.template.templateString).distinct
  
  val valueStrings = absCluster.map(abs => abs.valueString).distinct
  
  val templatePairs: Set[(String, String)] = {
    val pairs = for (q1 <- absCluster; q2 <- absCluster; if q1 != q2) yield List(q1, q2)
    val allPairs = pairs flatMap {
      case List(q1, q2) if q1.valueString == q2.valueString && q1.template.templateString != q2.template.templateString => Some((q1.template.templateString, q2.template.templateString))
      case _ => None
    }
    allPairs.toSet
  }

}