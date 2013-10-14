package edu.knowitall.execution.synonyms
import edu.knowitall.execution.Search.{rel, arg1, arg2}
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.Search.Conjunction
import edu.knowitall.execution.Search.FieldKeywords
import edu.knowitall.execution.Search.FieldPhrase
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.Utils
import scala.Option.option2Iterable

case class RelationRewrite(left: String, right: String, inverse: Boolean) {
  
  def apply(cq: ConjunctiveQuery): List[ConjunctiveQuery] = {
    val newCs = cq.conjuncts.map(apply(_))
    val conjSets = Utils.cartesian[TConjunct](newCs).toList
    conjSets.map(cs => ListConjunctiveQuery(cq.qVars, cs.toList))
  }
  
  def apply(c: TConjunct): List[TConjunct] = {
    c.values.get(rel) match {
      case Some(l: UnquotedTLiteral) => expand(l.value, c)
      case _ => List(c)
    } 
  }
  
  def expand(s: String, c: TConjunct): List[TConjunct] = {
    if (s.toLowerCase() == left) {
      val vals = c.values
      val newRel = UnquotedTLiteral(right)
      val newVals = for (x <- vals.get(arg1); y <- vals.get(arg2))
        yield if (inverse) {
          vals + (arg1 -> y) + (arg2 -> x) + (rel -> newRel)
        } else {
          vals + (arg2 -> y) + (arg1 -> x) + (rel -> newRel)
        }
      newVals match {
        case Some(x) => List(c, TConjunct(c.name, newVals.get))
        case _ => List(c)
      }
    } else {
      List(c)
    }
  }
}

abstract class RelationSynonyms {
  def getRewrites(left: String): List[RelationRewrite]
}

case class TriplestoreRelationSynonyms(client: TriplestoreClient, max: Int = 10) extends RelationSynonyms {
  
  val synRel = "berant_clear_gt13"
  val stemmer = new MorphaStemmer
  val tokenizer = new ClearTokenizer
  
  def preprocess(s: String): String = 
    tokenizer.tokenize(s).map(stemmer.stemToken).map(_.lemma).mkString(" ").toLowerCase()
  
  def relToQuery(r: String): TSQuery = {
    val left = preprocess(r)
    Conjunction(FieldKeywords(arg1, left), FieldPhrase(rel, synRel))
  }
  
  def fixRight(r: String): String = if (r.startsWith("be ")) r.replaceFirst("be ", "") else r
  
  def getRewrites(left: String): List[RelationRewrite] = {
    val q = relToQuery(left)
    val tuples = client.search(q)
    val score = (t: Tuple) => t.getFloat("conf_f") match {
      case Some(f: Float) => -f
      case _ => 0.0f
    }
    val sorted = tuples.sortBy(score)
    val result = for (t <- tuples; right <- t.getString("arg2"); 
                      inv <- t.getBoolean("inverse_b"))
      yield RelationRewrite(left, fixRight(right), inv)
    result.distinct.take(max)
  }
} 