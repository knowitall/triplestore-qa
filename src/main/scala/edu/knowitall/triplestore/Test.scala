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
  
 
  var tbl =
    Project(OnTripleCols,
    		Join(AttrsSim("contains.arg1", "antioxidant.arg1", 0.9), 
    				SearchFor("contains", RelEq("contains"), Arg2Eq("antioxidants")),
    				SearchFor("kills", RelCont("kills"))))
      
      	
  ptb(tbl)

}