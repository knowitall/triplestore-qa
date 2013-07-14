package edu.knowitall.triplestore

import Search.Query
import Tabulator.{tuplesToTable => toTable}
import jline.console.ConsoleReader
import edu.knowitall.qa._
import scopt.OptionParser

class QARepl(val parser: WeightedParser, val maxDerivations: Int = 5, url: String = "http://rv-n12.cs.washington.edu:8983/solr/triplestore", hits: Int = 100) {
  
  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)
  
  import planning._
  import Search.{Arg1Cont, RelCont, Arg2Cont}
  import Conditions._
  import Search.Conjunction
  
  private val splitRegex = "\\s+".r
  
  def derivations(question: String) = 
    parser.parse(splitRegex.split(question) map QWord.qWordWrap).toSeq
  
  def queryFor(derivation: Derivation) = {
    val squery = derivation.query
    val EntityCont = squery.queryField match {
      case Arg1 => Arg2Cont
      case Arg2 => Arg1Cont
    }
    def cleanName(str: String) = str.replaceAll("-", " ").dropRight(2)
    
    Conjunction(RelCont(cleanName(squery.relation)), EntityCont(cleanName(squery.entity)))
  }
  
  def search(query: Query) = {
    val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace")
    Project(projection, ExecQuery("r", query))
  }
  
  def eval(input: String) = toTable(derivations(input).map(queryFor).distinct.map(search).flatten)
}

object QARepl extends App {
  
  import java.io.File
  import edu.knowitall.qa.EvalLexiconLoader
  
  case class Config(solrUrl: String = ".")
  
  val parser = new OptionParser[Config]("QARepl") {
    arg[String]("solrUrl") action { (x, c) =>
    c.copy(solrUrl = x) } text("URL of Solr Lexicon")
  }
  
  parser.parse(args, Config()) map { config =>
    
    val lexicon = new SolrLexicon(config.solrUrl)
    
    val parser = new WeightedBottomUpParser(lexicon)

    val repl = new QARepl(parser)

    val reader = new ConsoleReader()
    while (true) {
      val line = reader.readLine("> ")
      println(repl.eval(line))
    }
  }
}