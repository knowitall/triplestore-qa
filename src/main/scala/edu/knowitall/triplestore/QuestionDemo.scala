package edu.knowitall.triplestore

import Search.Query
import Tabulator.{ tuplesToTable => toTable }
import jline.console.ConsoleReader
import edu.knowitall.qa._
import scopt.OptionParser
import org.slf4j.LoggerFactory
import unfiltered.filter.Intent
import unfiltered.response.ResponseString
import unfiltered.response.ResponseBytes
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
import unfiltered.response.ContentEncoding
import org.apache.commons.io.IOUtils

class QARepl(val parser: WeightedParser, val maxDerivations: Int = 10, url: String = "http://rv-n12.cs.washington.edu:8983/solr/triplestore", hits: Int = 100) {

  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)

  import planning._
  import Search.{ Arg1Cont, RelCont, Arg2Cont }
  import Conditions._
  import Search.Conjunction

  private val splitRegex = "\\s+".r

  def derivations(question: String) =
    parser.parse(splitRegex.split(question) map QWord.qWordWrap).toSeq.distinct

  def queryFor(derivation: Derivation): Option[Query] = {
    val squery = derivation.query
    val EntityCont = squery.queryField match {
      case Arg1 => Arg2Cont
      case Arg2 => Arg1Cont
    }
    def cleanName(str: String) = str.replaceAll("-", " ").dropRight(2)

    val cleanRel = cleanName(squery.relation)
    val cleanEnt = cleanName(squery.entity)

    if (cleanRel.nonEmpty && cleanEnt.nonEmpty)
      Some(Conjunction(RelCont(cleanName(squery.relation)), EntityCont(cleanName(squery.entity))))
    else None
  }

  def search(query: Query) = {
    val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace")
    Project(projection, ExecQuery("r", query))
  }

  def eval(input: String) = {
    val derivs = derivations(input).take(maxDerivations)
    val queries = derivs.map(d => (d, queryFor(d)))
    val results = queries.map { case (deriv, query) => (deriv, query, query.map(search).getOrElse(Nil)) }
    val allTuples = results.flatMap(_._3)

    val table = toTable(allTuples)
    val derivStrings = results.zipWithIndex.flatMap {
      case ((deriv, queryOpt, results), index) =>
        Seq(
          s"Derivation $index: wt=%.03f".format(deriv.weight),
          deriv.toString,
          "Query: " + queryOpt.map(_.toString).getOrElse("(query error)"),
          "Results: " + results.size.toString,
          "", "")
    }
    (Seq(table, "", s"TOP $maxDerivations DERIVATIONS:") ++ derivStrings).mkString("\n")
  }
}

object WebQuestionRepl extends App {

  val logger = LoggerFactory.getLogger(this.getClass)

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
      val result = repl.eval(query)
      ResponseString(result) ~> Ok
    }

    def getStatic = {
      ResponseString("""<html>
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
    help("help") text ("Starts a REPL web interface for entering natural language questions against the triplestore.")
    opt[Int]('p', "port") action { (x, c) => c.copy(port = x) } text ("webserver port")
  }

  argParser.parse(args, Config()) map { config =>
    val port = config.port
    Http(port).filter(Plan).run()
    logger.info(s"Starting webserver at http://localhost:$port")
  }
}