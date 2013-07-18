package edu.knowitall.scoring

import edu.knowitall.execution.AnswerGroup

import edu.knowitall.tool.conf.FeatureSet

object AnswerGroupFeatures {

  import edu.knowitall.tool.conf.Feature
  import edu.knowitall.tool.conf.Feature.booleanToDouble
  import edu.knowitall.tool.conf.FeatureSet
  import scala.collection.immutable.SortedMap
  
  type AnswerGroupFeature = Feature[AnswerGroup, Double]
  
  /**
   * Generic features that apply to any AnswerGroup
   */
  val features: Seq[AnswerGroupFeature] = Seq(
      
    Feature.from[AnswerGroup, Boolean]("Has answers from ReVerb", { group => 
      group.derivations.find(_.etuple.tuple.get("namespace").exists(_ == "reverb")).nonEmpty 
    }),
  
    Feature.from[AnswerGroup, Boolean]("Has answers from Freebase", { group => 
      group.derivations.find(_.etuple.tuple.get("namespace").exists(_ == "freebase")).nonEmpty 
    }),
    
    Feature.from[AnswerGroup, Boolean]("Has alternate surface forms", { group => 
      group.alternates.size >= 2
    }),
    
    Feature.from[AnswerGroup, Boolean]("Has more than one AnswerDerivation", { group => 
      group.derivations.size >= 2
    }),
    
    Feature.from[AnswerGroup, Boolean]("Join fields agree by fbid", { group => 
      group.derivations.groupBy(_.etuple.equery).forall { case (query, derivs) => joinsAgreeByFbid(query, derivs) }
    })
  )
  
  def featureSet = FeatureSet(features)
  
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