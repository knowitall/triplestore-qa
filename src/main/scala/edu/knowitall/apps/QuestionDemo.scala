package edu.knowitall.triplestore

import Search.TSQuery
import Tabulator.{ tuplesToTable => toTable }
import jline.console.ConsoleReader
import edu.knowitall.qa._
import scopt.OptionParser
import org.slf4j.LoggerFactory
import unfiltered.filter.Intent
import unfiltered.response.ResponseString
import unfiltered.response.ResponseStreamer
import unfiltered.response.ContentType
import unfiltered.response.Ok
import unfiltered.jetty.Http
import unfiltered.request.GET
import unfiltered.request.POST
import unfiltered.request.Path
import unfiltered.request.Seg
import unfiltered.response.NotFound
import unfiltered.request.Mime
import java.io.File
import java.io.OutputStream
import java.io.PrintStream
import unfiltered.response.ContentEncoding
import org.apache.commons.io.IOUtils
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.postag.PostaggedToken

class QARepl(val parser: WeightedParser, val maxDerivations: Int = 10, url: String = "http://rv-n12.cs.washington.edu:8983/solr/triplestore", hits: Int = 100) {

  val client = SolrClient(url, hits)
  val planning = TriplestorePlan(client)

  import planning._
  import Search.{ Arg1Cont, RelCont, Arg2Cont }
  import Conditions._
  import Search.Conjunction

  private val splitRegex = "\\s+".r

  def derivations(question: String) =
    parser.parse(splitRegex.split(question) map QWord.qWordWrap).toSeq.distinct

  def queryFor(derivation: WeightedDerivation): Option[TSQuery with Weight] = {
    val squery = derivation.query

    def cleanName(str: String) = str.replaceAll("-", " ").dropRight(2)

    val cleanRel = cleanName(squery.relation)
    val cleanEnt = cleanName(squery.entity)
    val qweight = derivation.weight

    if (cleanRel.nonEmpty && cleanEnt.nonEmpty)
      Some(queryFor(squery.queryField, cleanRel, cleanEnt, qweight))
    else None
  }
  
  def queryFor(queryField: Arg, rel: String, ent: String, qweight: Double) = {
    val EntityCont = queryField match {
      case Arg1 => Arg2Cont
      case Arg2 => Arg1Cont
    }
    new Conjunction(RelCont(rel), EntityCont(ent)) with Weight { val weight = qweight }
  }

  def search(query: TSQuery with Weight) = {
    val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace", "r.wt")
    val qweightStr = "%.02f".format(query.weight)
    val qweight = Tuple(Map("r.wt" -> qweightStr))
    val results = ExecQuery("r", query)
    
    Project(projection, results map {_.join(qweight)})
  }
  
  
  
  val postagger = new ClearPostagger
  
  val stemmer = new MorphaStemmer
  
  def eval(input: String): String = eval(postagger.postag(input).map(stemmer.lemmatizePostaggedToken))
  
  def eval(input: Seq[Lemmatized[PostaggedToken]]): String = {
    val inputStr = input.map(_.lemma).mkString(" ")
    val derivs = derivations(inputStr).take(maxDerivations)
    val queries = derivs.map(d => (d, queryFor(d)))
    val results = queries.map { case (deriv, query) => (deriv, query, query.map(search).getOrElse(Nil)) }
    val allTuples = results.flatMap(_._3)

    val table = if (allTuples.nonEmpty) toTable(allTuples) else "NO RESULTS\n"
    val derivStrings = results.zipWithIndex.flatMap {
      case ((deriv, queryOpt, results), index) =>
        Seq(
          s"Derivation $index",
          deriv.toString,
          "Query: " + queryOpt.map(_.toString).getOrElse("(query error)"),
          "Results: " + results.size.toString,
          "", "")
    }
    (Seq(table, "", s"TOP $maxDerivations DERIVATIONS:") ++ derivStrings).mkString("\n")
  }
}

object QuestionDemo extends App {

  val logger = LoggerFactory.getLogger(QuestionDemo.this.getClass)

  /**
   * This object defines the web server behavior.
   */
  object Plan extends unfiltered.filter.Plan {

    val lexicon = new SolrLexicon()

    val parser = new WeightedBottomUpParser(lexicon)

    val repl = new QARepl(parser)

    def intent = Intent {
      /**
       * Map the URL /query to the runQuery function.
       */
      case req @ POST(Path(Seg(Nil))) =>
        runQuery(req.parameterValues("q").mkString(" "))
      /**
       * Map all other URL paths to get the static content stored as a
       * resource on the classpath.
       */
      case req @ GET(Path(Seg(Nil))) => getStatic
    }

    def runQuery(query: String) = {

      new ResponseStreamer() {
        def stream(stream: OutputStream) = {
          val printStream = new PrintStream(stream)
          try {
            printStream.println(repl.eval(query))
          } catch {
            case e: Exception => e.printStackTrace(printStream)
          }
        }
      } ~> Ok
    }

    def getStatic = {
      ResponseString("""<html>
              <meta charset="utf-8"/>
              <h1>Question Derivation Search</h1>
              <body>
              <form method="POST">
              Enter a question:<input type="text" name="q"/><br/>
              <input type="submit"/>
            </form>
            </body></html>""") ~> Ok
    }
  }
  /**
   * Command line parameter parsing stuff.
   */
  case class Config(port: Int = 8080)

  val argParser = new scopt.OptionParser[Config]("scopt") {
    help("help") text ("Starts a simple web interface for entering natural language questions against the triplestore.")
    opt[Int]('p', "port") action { (x, c) => c.copy(port = x) } text ("webserver port")
  }

  argParser.parse(args, Config()) map { config =>
    val port = config.port
    Http(port).filter(Plan).run()
    logger.info(s"Starting webserver at http://localhost:$port")
  }
}