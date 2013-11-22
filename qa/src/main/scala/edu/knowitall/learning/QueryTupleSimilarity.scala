package edu.knowitall.learning

import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.Tuple
import edu.knowitall.tool.stem.MorphaStemmer
import scala.Option.option2Iterable

object QueryTupleSimilarity {
  
  def normalize(ss: List[String]): List[String] = ss.map(_.toLowerCase()).map(MorphaStemmer.stem)
  
  def tokenize(s: String) = s.split(" ")
  
  def queryWords(q: ConjunctiveQuery, t: Tuple): List[String] = {
    val literalFields = for (c <- q.conjuncts; (field, literal) <- c.literalFields) yield literal.value
    normalize(literalFields.flatMap(tokenize))
  }
  
  def tupleWords(q: ConjunctiveQuery, t: Tuple): List[String] = {
    val values = for (c <- q.conjuncts;
    				  (field, literal) <- c.literalFields;
    				  value <- t.getString(s"${c.name}.${field}"))
    				yield value
    normalize(values.flatMap(tokenize))
  }
  
  def jaccard(x: List[String], y: List[String]): Double = {
    val xset = x.toSet
    val yset = y.toSet
    if (x.size > 0 || y.size > 0) {
      xset.intersect(yset).size.toDouble / xset.union(yset).size
    } else {
      0.0
    }
  }
  
  def similarity(q: ConjunctiveQuery, t: Tuple): Double = {
    val qws = queryWords(q, t)
    val tws = tupleWords(q, t)
    jaccard(qws, tws)
  }

}