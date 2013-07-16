package edu.knowitall.apps
import jline.console.ConsoleReader
import scopt.OptionParser
import edu.knowitall.execution.Conditions.On
import edu.knowitall.execution.Search.Arg1Cont
import edu.knowitall.execution.Search.Arg2Cont
import edu.knowitall.execution.Search.Conjunction
import edu.knowitall.execution.Search.RelCont
import edu.knowitall.execution.Tabulator.{tuplesToTable => toTable}
import edu.knowitall.parsing.Parser
import edu.knowitall.parsing.BottomUpParser
import edu.knowitall.parsing.QWord
import edu.knowitall.parsing.Derivation
import edu.knowitall.parsing.{Arg1, Arg2}
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.triplestore.TriplestorePlan
import java.io.File

class QARepl(val parser: Parser, val maxDerivations: Int = 5, url: String = "http://rv-n12:8983/solr/triplestore", hits: Int = 100) {
  
  val client = SolrClient(url, hits)
  val planning = TriplestorePlan(client)
  
  import planning._
  import edu.knowitall.execution.Search.{Arg1Cont, RelCont, Arg2Cont}
  import edu.knowitall.execution.Conditions._
  import edu.knowitall.execution.Search.Conjunction
  
  private val splitRegex = "\\s+".r
  
  def derivations(question: String) = 
    parser.parse(splitRegex.split(question) map QWord.qWordWrap).toSeq.distinct.take(maxDerivations)
  
  def queryFor(derivation: Derivation) = {
    val squery = derivation.query
    val EntityCont = squery.queryField match {
      case Arg1 => Arg2Cont
      case Arg2 => Arg1Cont
    }
    Conjunction(RelCont(squery.relation), EntityCont(squery.entity))
  }
  
  def search(derivation: Derivation) = {
    val projection = On("r.arg1", "r.rel", "r.arg2", "r.namespace")
    Project(projection, ExecQuery("r", queryFor(derivation)))
  }
  
  def eval(input: String) = derivations(input).zipWithIndex.flatMap { case (deriv, index) => 
      Seq(s"Derivation #$index", deriv.toString, "Results:", toTable(search(deriv)))
    } mkString("\n")
}

object QARepl extends App {
  
  import java.io.File
  import edu.knowitall.parsing.LexiconLoader
  
  case class Config(dataPath: File = new File("."))
  
  val parser = new OptionParser[Config]("QARepl") {
    arg[File]("path") action { (x, c) =>
    c.copy(dataPath = x) } text("Path to Paralex Evaluation Data")
  }
  
  parser.parse(args, Config()) map { config =>
  
  val dbVocab = new File(config.dataPath, "database/vocab.txt")
  val lexVocab = new File(config.dataPath, "lexicons/paralex/vocab.txt")
  val lexItems = new File(config.dataPath, "lexicons/paralex/lexicon.txt")
  
  val lexicon = new LexiconLoader(dbVocab, lexVocab, lexItems).load
  
  val parser = new BottomUpParser(lexicon)

  val repl = new QARepl(parser)

  val reader = new ConsoleReader()
  while (true) {
    val line = reader.readLine("> ")
    println(repl.eval(line))
  }
  }
}