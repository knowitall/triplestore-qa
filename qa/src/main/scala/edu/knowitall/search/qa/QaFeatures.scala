package edu.knowitall.search.qa
import edu.knowitall.paraphrasing.template.TemplatePair
import edu.knowitall.execution.ExecTuple
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.learning.SparseVector
import edu.knowitall.learning.QueryTupleSimilarity
import com.typesafe.config.ConfigFactory
import edu.knowitall.lm.KenLmServer
import edu.knowitall.util.NlpUtils
import edu.knowitall.execution.Search
import edu.knowitall.relsyn.RelSynRule
import com.rockymadden.stringmetric.StringMetric
import edu.knowitall.execution.Tuple
import edu.knowitall.paralex.ParalexRecord

object QaFeatures extends Function[QaStep, SparseVector] {
  
  val conf = ConfigFactory.load()
  val defaultPmi = conf.getDouble("paraphrase.defaultPmi")
  val defaultLm = conf.getDouble("paraphrase.defaultLm")
  val lmClient = new KenLmServer()
  
  val answerIsLinked = ExecutionFeature { (question: String, etuple: ExecTuple) =>
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
  
  val tupleNamespace = ExecutionFeature { (q: String, etuple: ExecTuple) => 
    val tuple = etuple.tuple
    val nss = tuple.attrs.keys.filter(_.endsWith(".namespace")).flatMap(tuple.getString(_))
    nss.map(ns => s"answer from namespace '$ns'")
  }
  
  val numConjuncts = ExecutionFeature { (q: String, etup: ExecTuple) =>
    ("num conjuncts" -> etup.query.conjuncts.size)
  }
  
  val numSteps = (step: QaStep) => SparseVector("steps" -> 0.25)
  
  val querySimilarity = ExecutionFeature { (q: String, etuple: ExecTuple) =>
    val query = etuple.query
    val tuple = etuple.tuple
    val relSim = QueryTupleSimilarity.relSimilarity(query, tuple)
    val argSim = QueryTupleSimilarity.argSimilarity(query, tuple)
    val quesSim = QueryTupleSimilarity.questionQuerySimilarity(query, q)
    SparseVector("evidence similarity with query (rels only)" -> relSim,
    			 "evidence similarity with query (args only)" -> argSim,
    			 "query similarity with question" -> quesSim)
  }
  
  def freebaseLink(key: String, tuple: Tuple) = tuple.getString(key + "_fbid_s")
  
  val joinSimilarity = ExecutionFeature { (q: String, etuple: ExecTuple) =>
    val query = etuple.query
    val tuple = etuple.tuple
    val sims = for {
      (key1, key2) <- query.joinPairs
      val1 <- tuple.getString(key1)
      val2 <- tuple.getString(key2)
      s <- StringMetric.compareWithDiceSorensen(val1, val2)(1)
    } yield s
    val minJoinSim = if (sims.isEmpty) 0.0 else sims.min
    
    val fbidPairs = for {
      (key1, key2) <- query.joinPairs
      fbid1 <- freebaseLink(key1, tuple)
      fbid2 <- freebaseLink(key2, tuple)
    } yield (fbid1, fbid2)
    
    val fbidViolation = if (fbidPairs.exists(pair => pair._1 != pair._2)) 1.0 else 0.0
    
    SparseVector(
        "minimum join key similarity" -> minJoinSim,
        "fbid join key violation" -> fbidViolation)
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
  
  def templateArgFeatures(step: QaStep): SparseVector = step.toState match {
    case s: AbstractedArgState => {
      val sent = s.processed
      val span = s.argInterval
      val tags = sent.postags.slice(span.start, span.end)
      val tagPat = tags.mkString(" ")
      Some(s"template arg pos tags = $tagPat")
    }
    case _ => SparseVector.zero
  }
  
  val prefixAndDate = ExecutionFeature { (q: String, etuple: ExecTuple) =>
    val prefix = NlpUtils.questionPrefix(q)
    val isDate = NlpUtils.isDate(etuple.answerString)
    if (isDate) {
      Some(s"question prefix = '$prefix' and isDate")
    } else {
      None
    }
  }
  
  val prefixAndShape = ExecutionFeature { (q: String, etuple: ExecTuple) =>
    val prefix = NlpUtils.questionPrefix(q)
    val shape = NlpUtils.stringShape(etuple.answerString, 4)
    s"question prefix = '$prefix' and answer shape = $shape"
  }
  
  val lightVerbRel = QueryFeature { (q: String, query: ConjunctiveQuery) =>
    val values = for {
      c <- query.conjuncts
      (field, literal) <- c.literalFields
      value = literal.value
      if field == Search.rel && NlpUtils.isLightVerb(value)
    } yield value
    if (values.isEmpty) {
      None
    } else {
      Some(s"query relation is light verb")
    }
  }
  
  val relSynFeatures = (step: QaStep) => step.action match {
    case rule: RelSynRule => {
      val passivized = rule.rel2 == rule.rel1 + " by" && rule.inverted
      val unpassivized = rule.rel1 == rule.rel2 + " by" && rule.inverted
      val result = if (passivized | unpassivized) 1.0 else 0.0
      SparseVector("relSynRule pmi" -> rule.pmi, "relSynRule passivized" -> result)
    }
    case _ => SparseVector.zero
  }
  
  val paralexScore = (step: QaStep) => step.action match {
    case r: ParalexRecord => SparseVector("paralex score" -> r.score)
    case _ => SparseVector.zero
  }
  
  def apply(s: QaStep) = actionType(s) +
		  				 answerIsLinked(s) +
		  				 tupleNamespace(s) +
		  				 querySimilarity(s) +
		  				 templatePairPmi(s) +
		  				 paraphraseLm(s) +
		  				 numConjuncts(s) +
		  				 prefixAndDate(s) +
		  				 lightVerbRel(s) +
		  				 relSynFeatures(s) +
		  				 numSteps(s) + 
		  				 templateArgFeatures(s) +
		  				 prefixAndShape(s) +
		  				 joinSimilarity(s) +
		  				 paralexScore(s)
  
}

case class TemplatePairFeature(f: Function2[String, TemplatePair, SparseVector]) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = step.action match {
    case a: TemplatePair => f(step.question, a)
    case _ => SparseVector.zero
  }
}

case class ExecutionFeature(f: Function2[String, ExecTuple, SparseVector]) extends Function[QaStep, SparseVector] {
  override def apply(step: QaStep) = step.toState match {
    case ts: TupleState => f(step.question, ts.execTuple)
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