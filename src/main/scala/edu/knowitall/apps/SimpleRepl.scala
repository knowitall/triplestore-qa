package edu.knowitall.apps

import jline.console.ConsoleReader
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.Joiner
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search.ProjectTriples
import edu.knowitall.execution.Tabulator.tuplesToTable

case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", 
    hits: Int = 500) {
  
  val client = SolrClient(url, hits)
  val joiner = Joiner(client)
  
  def toTable(ts: List[Tuple], n: Integer): String = {
    val colSuffs = List("arg1", "rel", "arg2", "namespace")
    val cols = for (i <- Range(0, n); s <- colSuffs) yield s"r$i.$s"
    tuplesToTable(cols.toList, ts)
  }
  
  def eval(input: String): String = {
    val qs = TConjunct.fromStringMult(input)
    val tuples = ProjectTriples(joiner.joinQueries(qs))
    return toTable(tuples.toList, qs.size)
  } 

}

object SimpleRepl extends App {
  
  override def main(args: Array[String]) = {
    import jline.console.ConsoleReader
    val repl = SimpleRepl()
    val reader = new ConsoleReader()
    while (true) {
	  val line = reader.readLine("> ")
	  val result = repl.eval(line)
  	  println(result)
    }
  }

}

object CommandLine extends App {
  override def main(args: Array[String]) = {
    val repl = SimpleRepl()
    val input = args.mkString(" ")
    println(repl.eval(input))
  }
}