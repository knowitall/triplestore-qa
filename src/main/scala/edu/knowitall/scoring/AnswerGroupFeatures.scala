package edu.knowitall.scoring

import edu.knowitall.execution.AnswerGroup

import edu.knowitall.tool.conf.FeatureSet

object AnswerGroupFeatures {

  import edu.knowitall.tool.conf.Feature
  import edu.knowitall.tool.conf.Feature.booleanToDouble
  import edu.knowitall.tool.conf.FeatureSet
  import scala.collection.immutable.SortedMap
  import TupleFeatures._
  
  type AG = AnswerGroup
  type AnswerGroupFeature = Feature[AnswerGroup, Boolean]
  
  def tuples(group: AG) = group.derivations.map(_.etuple.tuple)
  def queryDerivations(group: AG) = group.derivations.groupBy(_.etuple.equery)
  
  /**
   * Generic features that apply to any AnswerGroup
   */
  val features: Seq[(String, AG => Boolean)] = Seq(
      
    ("Has answers from ReVerb", { group: AG => tuples(group) exists isFromNamespace("reverb") }),
  
    ("Has answers from Freebase", { group: AG => tuples(group) exists isFromNamespace("freebase") }),
    
    ("Has alternate surface forms", { group: AG => group.alternates.size >= 2 }),
    
    ("Has more than one AnswerDerivation", { group: AG => group.derivations.size >= 2 }),
    
    ("Join fields agree by fbid", { group: AG => queryDerivations(group) forall (joinsAgreeByFbid _).tupled })
  )
  
  def featureSet: FeatureSet[AG, Double] = FeatureSet(features.map(p => booleanToDouble(Feature.from(p._1, p._2))))
  
  // Feature Helper methods
  import edu.knowitall.execution.ExecQuery
  import edu.knowitall.execution.ExecConjunctiveQuery
  import edu.knowitall.execution.AnswerDerivation
  import edu.knowitall.execution.TVariable
  
  def joinsAgreeByFbid(equery: ExecQuery, derivs: Seq[AnswerDerivation]): Boolean = {
    
    // get fields for each query variable
    val tvars = equery match {
      case ExecConjunctiveQuery(uquery, query) => 
        query.conjuncts.flatMap(c => c.joinKeys).groupBy(_._1).map { case (tvar, pairs) => (tvar, pairs.map(_._2)) }
      case _ => Map.empty[TVariable, Seq[String]]
    }
    
    val tuples = derivs.map(_.etuple.tuple)
    
    tvars.forall { case (tvar, keys) => 
      val fbidKeys = keys.map(k => k + "_fbid_s")
      // get a list of fbids that appear in these fields
      val fbids = fbidKeys.flatMap(k => tuples.flatMap(_.get(k).filter(_.isInstanceOf[String]).map(_.asInstanceOf[String])))
      // filter out fbids that are substrings of other fbids (e.g. 0667v gets filtered out if ns:m.0667v is present)
      val distinctFbids = fbids.filterNot(fbid => fbids.exists(fbid2 => fbid2.contains(fbid) && fbid2 != fbid)).toSet
      // if there are zero or one distinct fbids, then they agree.
      distinctFbids.size <= 1
    }
  }
}

object TupleFeatures {
  
  import edu.knowitall.execution.Tuple
  
  def isFromNamespace(ns: String)(tuple: Tuple) = tuple.get("namespace").exists {
    case s: String => s.contains(ns)
    case _ => false
  }
}