package edu.knowitall.triplestore

import Search.Query
import Tabulator.{tuplesToTable => toTable}
import jline.console.ConsoleReader
import edu.knowitall.qa._
import scopt.OptionParser

class QARepl(val parser: WeightedParser, val maxDerivations: Int = 10, url: String = "http://rv-n12.cs.washington.edu:8983/solr/triplestore", hits: Int = 100) {
  
  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)
  
  import planning._
  import Search.{Arg1Cont, RelCont, Arg2Cont}
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
    val derivStrings = results.zipWithIndex.flatMap { case ((deriv, queryOpt, results), index) =>
      Seq(
          s"Derivation $index: wt=%.03f".format(deriv.weight),
          deriv.toString, 
          "Query: " + queryOpt.map(_.toString).getOrElse("(query error)"),
          "Results: " + results.size.toString,
          "", ""
        )
    }
    (Seq(table, "", s"TOP $maxDerivations DERIVATIONS:") ++ derivStrings).mkString("\n")
  }
}

object QARepl extends App {
  
  import java.io.File
  import edu.knowitall.qa.EvalLexiconLoader

  val lexicon = new SolrLexicon()

  val parser = new WeightedBottomUpParser(lexicon)

  val repl = new QARepl(parser)

  val reader = new ConsoleReader()
  while (true) {
    val line = reader.readLine("> ")
    println(repl.eval(line))
  }
}