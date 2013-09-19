package edu.knowitall.paralex

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.BasicAnswerGrouper
import edu.knowitall.scoring.UniformAnswerScorer
import edu.knowitall.apps.QASystem
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.execution.SimpleQuery
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.parsing.QWord
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.parsing.QToken
import edu.knowitall.execution.Search
import edu.knowitall.parsing.ArgVar
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.execution.Search.FieldPhrase
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.execution.Search.FieldKeywords
import scala.util.Random
import scala.io.Source
import scala.collection.immutable.HashMap
import shapeless.ToList
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.scoring.NumDerivationsScorer

case class QACluster(answers: Seq[String], questions: Seq[String])

case object QACluster {
  val prefix = "^([qa]:)(.*)".r
  def trim(s: String) = s match {
    case prefix(l, r) => r
    case _ => s
  }
  def fromString(s: String): QACluster = {
    val parts = s.split("\t").toSeq
    val questions = parts.filter(p => p.startsWith("q:")).map(trim).distinct
    val answers = parts.filter(p => p.startsWith("a:")).map(trim).distinct
    QACluster(answers, questions)
  }
}

case class OutputRecord(template1: Seq[QToken], template2: Seq[QToken]) {
  def stem(tpl: Seq[QToken]) = tpl.map(qt => qt match {
    case QWord(w) => QWord(MorphaStemmer.stem(w))
    case x => x
  }).mkString(" ")
  val stemmedTemplate1 = stem(template1)
  val stemmedTemplate2 = stem(template2)
  override def toString = stemmedTemplate1 + "\t" + stemmedTemplate2
  def ordered = if (stemmedTemplate1 < stemmedTemplate2) {
    OutputRecord(template1, template2)
  } else {
    OutputRecord(template2, template1)
  }
}

case class ParsedQuestion(string: String, qWords: List[QWord], parses: List[TConjunct])

case class AbstractedArg(arg: List[QWord], conjunct: TConjunct)

case class TemplateGenerator(parser: QuestionParser = RegexQuestionParser(),
    tokenizer: Tokenizer = new ClearTokenizer()) {
  
  def filterQuery(uq: UQuery): Option[TConjunct] = uq match {
    case ListConjunctiveQuery(List(v), List(c)) => Some(TConjunct(c.name, c.values))
    case _ => None
  }
  
  def getArg(sq: TConjunct): AbstractedArg = {
    val fields = sq.literalFields
    val fields2 = for ((f, v) <- fields; if f == Search.arg1 || f == Search.arg2) 
      yield (f,v)
    val argField = fields2 match {
      case List((Search.arg1, v)) => Search.arg1
      case List((Search.arg2, v)) => Search.arg2
      case _ => throw new IllegalStateException(s"Could not find non-variable arg in $fields")
    }
    val arg = toQWords(sq.values(argField).toString().toLowerCase())
    val newVals = sq.values + (argField -> TVariable("y"))
    val newConj = TConjunct(sq.name, newVals)
    AbstractedArg(arg, newConj)
  }
  
  def toQWords(s: String) = tokenizer.tokenize(s).map(t => QWord(t.string)).toList
  
  def parseQuestion(q: String): ParsedQuestion = {
    val parses = parser.parse(q).flatMap(filterQuery).toList
    val qWords = toQWords(q.toLowerCase())
    ParsedQuestion(q, qWords, parses)
  }
  
  def getArgs(pq: ParsedQuestion): List[AbstractedArg] = pq.parses.map(getArg)
  
  def abstractArg(pq: ParsedQuestion, arg: List[QWord]): List[QToken] = {
    val qw = pq.qWords
    val i = qw.indexOfSlice(arg)
    if (i >= 0) {
      qw.slice(0, i) ++ List(ArgVar) ++ qw.slice(i+arg.size, qw.size)
    } else {
      pq.qWords
    }
  }
  
  def isAbstracted(aq: List[QToken]) = aq.count(t => t == ArgVar) == 1
  
  def generateTemplates(c: QACluster): List[OutputRecord] = {
    try {
      val qstrs = Random.shuffle(c.questions.filter(_.size < 80)).take(25)
      val qs = qstrs.map(parseQuestion) 
      val records = 
        for (q1 <- qs;
        	 arg <- getArgs(q1);
        	 abs1 = abstractArg(q1, arg.arg);
        	 q2 <- qs;
        	 abs2 = abstractArg(q2, arg.arg);
        	 if isAbstracted(abs2)) 
          yield OutputRecord(abs1, abs2).ordered
      records.toList.distinct.filter(a => a.template1 != a.template2)
    } catch {
      case e: Throwable => {
        e.printStackTrace(System.err)
        List()
      }
    }
  }
  
}

trait QuestionParaphraser {
  def paraphrase(s: String, k: Int): List[String]
}

case class MapQuestionParaphraser(map: Map[String, List[(String, Double)]]) extends QuestionParaphraser {
  val maxSize = 4
  val tokenizer = new ClearTokenizer()
  def normalize(s: String): Seq[QWord] = tokenizer(s.toLowerCase).map(MorphaStemmer.stemToken).map(t => QWord(t.lemma))
  def intervals(size: Int, max: Int) =
    for (i <- Range(0, size); j <- Range(i, size); if j+1-i <= max) yield (i, j+1)
    
    case class Abstracted(arg: Seq[QWord], template: Seq[QToken]) { 
      def templateString = template.mkString(" ")
      def substitute = { for (q <- template) yield q match {
        case QWord(x) => List(QWord(x))
        case ArgVar => arg
      }}.flatten
    }
    
  def templatize(ws: Seq[QWord]): Seq[Abstracted] = {
      val n = ws.size
      for ((i, j) <- intervals(n, maxSize);
    	   left = ws.slice(0, i);
    	   right = ws.slice(j, n);
    	   arg = ws.slice(i, j);
    	   abs = Abstracted(arg, left ++ List(ArgVar) ++ right)
    	   ) yield abs
  }
  def templateToQTokens(s: String) = {
    val toks = s.split(" ")
    val i = toks.indexOf("$y")
    val n = toks.size
    if (i >= 0) {
      val left = toks.slice(0, i).map(x => QWord(x)).toList
      val right = toks.slice(i+1, n).map(x => QWord(x)).toList
      left ++ List(ArgVar) ++ right 
    } else {
      toks.map(x => QWord(x)).toList
    }
  }
  
  override def paraphrase(s: String, k: Int) = {
    val q = normalize(s)
    val temps = templatize(q)
    val matches = for (t <- temps;
    				items = map.getOrElse(t.templateString, List());
    				(pp, score) <- items) yield (Abstracted(t.arg, templateToQTokens(pp)).substitute, score)
    matches.sortBy(x => -x._2).map(x => x._1.mkString(" ")).toList
  }
}
object MapQuestionParaphraser {
  def fromFile(path: String) = {
    val iter = Source.fromFile(path, "UTF8").getLines.grouped(1000)
    val items = for (g <- iter; line <- g.par; item <- processLine(line)) yield item
    MapQuestionParaphraser(items.toMap)
  }
  def processLine(s: String): Option[(String, List[(String, Double)])] = {
    val fields = s.split("\t").toList
    fields match {
      case key :: rest => Some((key, toPairs(rest)))
      case _ => None
    }
  }
  def parseDouble(s: String) = try { Some(s.toDouble) } catch { case e:Throwable => None }
  def toPairs(rest: List[String]): List[(String, Double)] = {
    rest.grouped(2).flatMap {
      pair => pair match {
        case List(a: String, b: String) => parseDouble(b) match {
          case Some(d) => Some(a, d)
          case _ => None
        }
        case _ => None
      }
    }.toList
  }
}

class ParalexQuestionParser(paraphraser: QuestionParaphraser, parser: QuestionParser, k: Int)
  extends QuestionParser {
  def parse(q: String): Iterable[UQuery] = {
    val questions = paraphraser.paraphrase(q, k) ++ List(q)
    for (pq <- questions; query <- parser.parse(pq)) yield query
  }
}

object FooBar extends App {
  val pp = MapQuestionParaphraser.fromFile("pmi_agg.txt")
  val base = new RegexQuestionParser()
  val parser = new ParalexQuestionParser(pp, base, 10)
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:10893/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 100000)
  val executor = IdentityExecutor(client)
  val grouper = BasicAnswerGrouper()
  val scorer = NumDerivationsScorer()
  val qa = QASystem(parser, executor, grouper, scorer)
  val question = args(0)
  println(s"Original question: $question")
  val answers = qa.answer(question).sortBy(-1*_.score)
  for (a <- answers) println(a.answer.mkString(" "))
  
}