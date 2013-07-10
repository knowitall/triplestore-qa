package edu.knowitall.triplestore
import Tabulator.{tuplesToTable => toTable}
import jline.console.ConsoleReader
import Search.ProjectTriples

case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", 
    hits: Int = 100) {
  
  val client = TriplestoreClient(url, hits)
  val joiner = Joiner(client)
  
  def eval(input: String): String = {
    val qs = AbstractQuery.fromStringMult(input)
    val tuples = ProjectTriples(joiner.joinQueries(qs))
    return toTable(tuples)
  } 

}

object SimpleRepl extends Application {
  
  val repl = SimpleRepl()
  val reader = new ConsoleReader()
  while (true) {
	  val line = reader.readLine("> ")
	  val result = repl.eval(line)
	  println(result)
  }

}