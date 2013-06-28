package edu.knowitall.triplestore
import Tabulator.{tuplesToTable => toTable}

object Test extends Application {
  
  val client = TriplestoreClient("http://localhost:8983/solr/triplestore", 100)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._
  import Operators._
  
  val ptb = (t: Tuples) => println(toTable(t))
  
  
  val result = //ProjectOn("r.arg1",
      Union(
          SearchFor("r", Rel("contains"), Arg2("protein")),
          SearchFor("r", Rel("high in"), Arg2("protein"))
      )
  //)
  
  ptb(result)
  
}