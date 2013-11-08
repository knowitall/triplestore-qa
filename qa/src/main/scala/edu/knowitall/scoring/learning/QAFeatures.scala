package edu.knowitall.scoring.learning

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.scoring.features.QueryTupleSimilarity
import edu.knowitall.paraphrasing.template.TemplateParaphraseDerivation
import com.typesafe.config.ConfigFactory
import edu.knowitall.util.NlpUtils

object QAFeatures extends Function[AnswerDerivation, SparseVector] {
  
  type Deriv = AnswerDerivation
  val conf = ConfigFactory.load()
  val defaultPmi = conf.getDouble("paraphrase.defaultPmi")
  val defaultLm = conf.getDouble("paraphrase.defaultLm")
  
  def apply(d: Deriv) = features(d)
  
  def paraphraseScore(d: Deriv): SparseVector = ("paraphrase score", d.paraphrase.derivation.score)
  
  def isParaphrased(d: Deriv): SparseVector = ("is paraphrased?", (d.paraphrase.source != d.paraphrase.target))
  
  def querySimilarity(d: Deriv): SparseVector = ("evidence similarity with query", QueryTupleSimilarity.similarity(d.parsedQuery, d.execTuple.tuple))
  
  def paraphrasePmi(d: Deriv): SparseVector = {
    val pmi = d.paraphrase.derivation match {
      case pd: TemplateParaphraseDerivation => pd.pmi
      case _ => defaultPmi
    }
    ("paraphrase pmi" -> pmi)
  }
  
  def paraphraseLm(d: Deriv): SparseVector = {
    val lm = d.paraphrase.derivation match {
      case pd: TemplateParaphraseDerivation => pd.lm
      case _ => defaultLm
    }
    ("paraphrase lm" -> lm)
  }
  
  def answerIsLinked(d: Deriv): SparseVector = {
    val tuple = d.execTuple.tuple
    val qAttrs = d.execTuple.query.qAttrs
    val isLinked = qAttrs.exists(attr => {
      tuple.get(attr + "_fbid_s") match {
        case Some(value) => true
        case _ => false
      } 
    })
    ("answer is linked to freebase", isLinked)
  }
  
  def prefixAndDate(d: Deriv): SparseVector = {
    val prefix = NlpUtils.questionPrefix(d.question)
    val isDate = NlpUtils.isDate(d.answerString)
    if (isDate) {
      Some(s"question prefix = '$prefix' and isDate")
    } else {
      None
    }
  }
  
  def namespaces(d: Deriv): SparseVector = {
    val tuple = d.execTuple.tuple
    val nss = tuple.attrs.keys.filter(_.endsWith(".namespace")).flatMap(tuple.getString(_))
    nss.map(ns => s"derivation contains namespace '$ns'")
  }
  
  def numConjuncts(d: Deriv): SparseVector = 
    ("number of query conjuncts" -> d.execTuple.query.conjuncts.size)
  
  def extractionCount(d: Deriv): SparseVector = {
    val tuple = d.execTuple.tuple
    val counts = tuple.attrs.keys.filter(_.endsWith(".num_extrs_i")).flatMap(tuple.getInt).filter(_ >= 1)
    val min = counts match {
      case counts if counts.size > 0 => counts.min
      case _ => 1
    }
    ("triple count" -> Math.log(min.toDouble))
  }
  
  
  val features = (d: Deriv) => isParaphrased(d) + 
		  					   querySimilarity(d) +
		  					   answerIsLinked(d) +
		  					   paraphrasePmi(d) +
		  					   paraphraseLm(d) +
		  					   paraphraseScore(d) +
		  					   namespaces(d) +
		  					   prefixAndDate(d) +
		  					   numConjuncts(d)
}




