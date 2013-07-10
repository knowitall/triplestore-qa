package edu.knowitall.triplestore
import Tabulator.tuplesToTable
import Search.ProjectTriples

case class SimpleRepl(url: String = "http://rv-n12:8983/solr/triplestore", 
    hits: Int = 100) {
  
  val client = TriplestoreClient(url, hits)
  val joiner = Joiner(client)
  
  def toTable(ts: List[Tuple], n: Integer): String = {
    val colSuffs = List("arg1", "rel", "arg2", "namespace")
    val cols = for (i <- Range(0, n); s <- colSuffs) yield s"r$i.$s"
    tuplesToTable(cols.toList, ts)
  }
  
  def eval(input: String): String = {
    val qs = AbstractQuery.fromStringMult(input)
    val tuples = ProjectTriples(joiner.joinQueries(qs))
    return toTable(tuples.toList, qs.size)
  } 

}

object SimpleRepl extends Application {
  
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

object CommandLine extends Application {
  override def main(args: Array[String]) = {
    val repl = SimpleRepl()
    val input = args.mkString(" ")
    println(repl.eval(input))
  }
}