package edu.knowitall.triplestore
import Tabulator.{tuplesToTable => toTable}

object Test extends Application {
  
  val client = TriplestoreClient("http://rv-n12:8983/solr/triplestore", 1000)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._
  import Operators.Union
  import com.rockymadden.stringmetric.similarity._
  
  val ptb = (t: Tuples) => println(toTable(t))
  
  val tbl1 = SearchFor("r", RelEq("is the mayor of"), Arg2Cont("new york"))

  val tbl2 = SearchFor("r", RelEq("is The Mayor of"), Arg2Cont("new york"))
  
  println("equals")
  ptb(tbl1)
  println
  println("contains")
  ptb(tbl2)
}