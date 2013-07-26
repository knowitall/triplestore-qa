package edu.knowitall.scoring

import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.triplestore.CountCachedTriplestoreClient
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.execution.ExecConjunctiveQuery
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution.Tuple
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.Feature.booleanToDouble
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.common.Resource.using
import scala.collection.immutable.SortedMap
import TupleFeatures._
import scala.language.implicitConversions
import io.Source

object AnswerGroupFeatures {

  type AnswerGroupFeature = Feature[AnswerGroup, Double]
  
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0
  
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val countCacheClient = CountCachedTriplestoreClient(baseClient, 10000)
  val client = CachedTriplestoreClient(countCacheClient, 10000)
  
  def allTuples(group: AnswerGroup)  = group.derivations.map(_.etuple.tuple)
  def allQueries(group: AnswerGroup) = group.derivations.map(_.etuple.equery).distinct
  
  object MultipleNamespaces extends AnswerGroupFeature("Answer comes from multiple namespaces") {
    def apply(group: AnswerGroup) = {
      def getNamespace(t: Tuple) = t.get("namespace").toString
      val tupleNamespaces = allTuples(group) map getNamespace
      val distinctNamespaces = tupleNamespaces.distinct
      distinctNamespaces.size.toDouble
    }
  }
  
  object NumberOfAlternates extends AnswerGroupFeature("Number of alternate forms of the answer") {
    def apply(group: AnswerGroup) = group.alternates.size.toDouble
  }
  
  object NumberOfDerivations extends AnswerGroupFeature("Number of AnswerDerivations in the AnswerGroup") {
    def apply(group: AnswerGroup) = {
      val numDerivations = group.derivations.size.toDouble
      math.sqrt(numDerivations)
    }
  }
  
  object AnswerContainsArticles extends AnswerGroupFeature("Answer contains article tokens") {
    val articles = Set("the", "a", "an")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      articles.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerStartsWithDeterminers extends AnswerGroupFeature("Answer contains determiner") {
    val determiners = Set("these", "those", "that", "this", "some", "most", "all", "any", "both", "either", "each", "more", "less")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").headOption.map(_.toLowerCase).toSet
      determiners.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object AnswerContainsNegation extends AnswerGroupFeature("Answer contains a negation word") {
    val negationWords = Set("no", "none", "never", "neither", "nobody", "nor", "nothing", "nowhere", "n't", "cannot", "cant", "can't", "wont")
    def apply(group: AnswerGroup) = {
      val firstAnswer = group.alternates.head.head
      val firstAnswerTokens = firstAnswer.split("\\s+").map(_.toLowerCase).toSet
      negationWords.intersect(firstAnswerTokens).nonEmpty
    }
  }
  
  object TriplestoreAnswerFrequency extends AnswerGroupFeature("Log of Frequency of answer in the triplestore") {
    val junkTokens = Set("a", "an", "the", "or", "and", "&") ++ AnswerStartsWithDeterminers.determiners
    val splitRegex = "\\s+".r
    
    case class CountQuery(arg: String) extends TSQuery {
      def toQueryString = {
        val answerTokens = splitRegex.split(arg)
        val filteredTokens = answerTokens.filter(tok => !junkTokens.contains(tok)).toSeq
        val escapedTokens = filteredTokens map SolrClient.escape
        val cleanAnswer = if (escapedTokens.nonEmpty) escapedTokens.mkString("\"", " ", "\"") else Seq("*")
        "arg1:\"%s\"".format(cleanAnswer)
      }
    }
    def apply(group: AnswerGroup) = {
      val rawAnswer = group.alternates.head.head.toLowerCase
      val query = CountQuery(rawAnswer)
      math.log(client.count(query).toDouble + 1)
    }
  }
  
  abstract class QueryFieldMatch(val subname: String) extends AnswerGroupFeature("Query fields match:" + subname) {
    
    import edu.knowitall.execution.{TLiteral, TConjunct}
    import edu.knowitall.execution.Search.TSField

    def normalizeLiteral(s: String): String
    
    /**
     * Test whether fields in qconj match corresponding fields in tuple.
     * Returns true if match, or if fields are absent
     */
    def queryFieldMatch(qconj: TConjunct, fields: Set[TSField], tuple: Tuple): Boolean = {

      val fieldStrings = fields.map(_.name)
      val existingLiteralFields = qconj.literalFields.filter { litfield => fieldStrings.contains(litfield._1) }
      val cleanedLiteralFields = existingLiteralFields.toSeq.map {
        case (field, literal) =>
          (field.name, normalizeLiteral(literal.toString))
      }
      // tuple fields corresponding to literal fields
      def fieldName(n: String) = s"${qconj.name}.$n"
      val tupleFieldNames = cleanedLiteralFields.map(_._1).map(fieldName)
      val tupleFields = tupleFieldNames.toSeq.map { name =>
        val value = tuple.getString(name).getOrElse(throw new RuntimeException(s"Tuple does not have field $name"))
        (name, normalizeLiteral(value))
      }
      require(cleanedLiteralFields.length == tupleFields.length)
      cleanedLiteralFields.zip(tupleFields).forall {
        case ((queryField, queryLiteral), (tupleField, tupleLiteral)) =>
          queryLiteral == tupleLiteral
      }
    }
  }
    
  object AnyRelExactMatch extends QueryFieldMatch(" Query relation exactly matches any tuple relation") {

    import edu.knowitall.execution.Search.rel
    import TriplestoreAnswerFrequency.junkTokens
    import TriplestoreAnswerFrequency.splitRegex
    import edu.knowitall.tool.stem.MorphaStemmer
    
    def normalizeLiteral(s: String) = {
      val lc = s.toLowerCase
      val split = splitRegex.split(lc)
      val stems = split.map(MorphaStemmer.apply)
      def tokenFilter(t: String) = !junkTokens.contains(t)
      val filtered = stems.filter(tokenFilter)
      filtered.mkString(" ")
    }
   
    def apply(group: AnswerGroup) = {
      val execConjQueryDerivs = group.derivations.filter(_.etuple.equery.isInstanceOf[ExecConjunctiveQuery])
      execConjQueryDerivs.exists { deriv =>
        val conjuncts = deriv.etuple.equery.asInstanceOf[ExecConjunctiveQuery].conjuncts
        conjuncts.exists { qconj => queryFieldMatch(qconj, Set(rel), deriv.etuple.tuple) }
      }
    }
  }
  
  /**
   * Generic features that apply to any AnswerGroup
   */
  val features: Seq[AnswerGroupFeature] = Seq(
      MultipleNamespaces, 
      NumberOfDerivations,
      AnswerContainsArticles,
      AnswerStartsWithDeterminers,
      AnswerContainsNegation,
      TriplestoreAnswerFrequency,
      AnyRelExactMatch)

  
  def featureSet: FeatureSet[AnswerGroup, Double] = FeatureSet(features)
}

object TupleFeatures {
  
  import edu.knowitall.execution.Tuple
  
  def isFromNamespace(ns: String)(tuple: Tuple) = tuple.get("namespace").exists {
    case s: String => s.contains(ns)
    case _ => false
  }
}