package edu.knowitall.search.qa
import edu.knowitall.paraphrasing.template.TemplatePair
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.scoring.learning.SparseVector
import edu.knowitall.scoring.learning.QueryTupleSimilarity
import com.typesafe.config.ConfigFactory
import edu.knowitall.lm.KenLmServer
import edu.knowitall.util.NlpUtils

object QaFeatures extends Function[QaStep, SparseVector] {
  
  val conf = ConfigFactory.load()
  val defaultPmi = conf.getDouble("paraphrase.defaultPmi")
  val defaultLm = conf.getDouble("paraphrase.defaultLm")
  val lmClient = new KenLmServer()
  
  val answerIsLinked = AnswerFeature { (question: String, etuple: ExecTuple) =>
    val tuple = etuple.tuple
    val qAttrs = etuple.query.qAttrs
    val isLinked = qAttrs.exists(attr => {
      tuple.get(attr + "_fbid_s") match {
        case Some(value) => true
        case _ => false
      } 
    })
    ("answer is linked to freebase", isLinked)
  }
  
  val actionType = ActionFeature { a: QaAction =>
    (s"action type = ${a.getClass.getSimpleName}", 1.0)
  }
  
  val tupleNamespace = AnswerFeature { (q: String, etuple: ExecTuple) => 
    val tuple = etuple.tuple
    val nss = tuple.attrs.keys.filter(_.endsWith(".namespace")).flatMap(tuple.getString(_))
    nss.map(ns => s"answer from namespace '$ns'")
  }
  
  val numConjuncts = QueryFeature { (q: String, query: ConjunctiveQuery) =>
    ("num conjuncts" -> query.conjuncts.size)
  }
  
  val querySimilarity = AnswerFeature { (q: String, etuple: ExecTuple) =>
    val query = etuple.query
    val tuple = etuple.tuple
    val sim = QueryTupleSimilarity.similarity(query, tuple)
    ("evidence similarity with query", sim)
  }
  
  val templatePairPmi = TemplatePairFeature { (q: String, pair: TemplatePair) =>
    ("template pair pmi", pair.pmi)
  }
  
  def paraphraseLm(step: QaStep): SparseVector = {
    step.toState match {
      case qs: QuestionState if qs.isParaphrased => ("paraphrase lm", lmClient.query(qs.question))
      case _ => SparseVector.zero
    }
  }
  
  val prefixAndDate = AnswerFeature { (q: String, etuple: ExecTuple) =>
    val prefix = NlpUtils.questionPrefix(q)
    val isDate = NlpUtils.isDate(etuple.answerString)
    if (isDate) {
      Some(s"question prefix = '$prefix' and isDate")
    } else {
      None
    }
  }
  
  def apply(s: QaStep) = actionType(s) +
		  				 answerIsLinked(s) +
		  				 tupleNamespace(s) +
		  				 querySimilarity(s) +
		  				 templatePairPmi(s) +
		  				 paraphraseLm(s) +
		  				 numConjuncts(s) +
		  				 prefixAndDate(s)
  
}

case class TemplatePairFeature(f: Function2[String, TemplatePair, SparseVector]) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = step.action match {
    case a: TemplatePair => f(step.question, a)
    case _ => SparseVector.zero
  }
}

case class AnswerFeature(f: Function2[String, ExecTuple, SparseVector]) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = step.toState match {
    case as: AnswerState => f(step.question, as.execTuple)
    case _ => SparseVector.zero
  }
}

case class QueryFeature(f: Function2[String, ConjunctiveQuery, SparseVector]) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = step.toState match {
    case qs: QueryState => f(step.question, qs.query)
    case _ => SparseVector.zero
  }
}

case class ActionFeature(f: QaAction => SparseVector) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = f(step.action)
}