package edu.knowitall.triplestore
import Search.Query
import Tabulator.{tuplesToTable => toTable}
import jline.console.ConsoleReader

case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", hits: Int = 100) {
  val client = TriplestoreClient(url, hits)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._
  val pat = "(.+),(.+),(.+)".r
  
  def toq(v: String, f: String => Query) = v match {
    case "?" => None
    case _ => Some(f(v.trim()))
  }
  
  def search(arg1: String, rel: String, arg2: String) = {
    val query = List(toq(arg1, Arg1Cont), toq(rel, RelCont), toq(arg2, Arg2Cont)).flatten
    Project(On("r.arg1", "r.rel", "r.arg2", "r.namespace"), SearchFor("r", query:_*))
  }
  
  def eval(input: String) = input match {
    case pat(arg1, rel, arg2) => toTable(search(arg1, rel, arg2))
    case _ => "Error."
  }
  
}

object SimpleRepl extends Application {
  
  val repl = SimpleRepl()
  val reader = new ConsoleReader()
  while (true) {
	  val line = reader.readLine("> ")
	  println(repl.eval(line))
  }

}