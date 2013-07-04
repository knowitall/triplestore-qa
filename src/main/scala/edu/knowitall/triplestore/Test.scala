package edu.knowitall.triplestore
import Tabulator.{tuplesToTable => toTable}

object Test extends Application {
  
  val client = TriplestoreClient("http://rv-n12:8983/solr/triplestore", 100)
  val planning = TriplestorePlan(client)
  import planning._
  import Conditions._
  import Operators.Union
  import com.rockymadden.stringmetric.similarity._
  
  val ptb = (t: Tuples) => println(toTable(t))
  
  
  val tbl = 
  Project(On("r1.arg1", "r1.rel", "r1.arg2", "r2.arg1", "r2.rel", "r2.arg2"),
      SearchJoin("r1.arg1", "r2.arg1", 
    		  SearchFor("r1", RelCont("is high in"), Arg2Cont("potassium")),
    		  PartialSearchFor("r2", RelEq("Type"), Arg2Eq("fruit"))))
    		  
   
    ptb(tbl)
}