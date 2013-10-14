package edu.knowitall.execution

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig

case class ConjunctTable(conjunct: TConjunct, tuples: List[Tuple]) {
  override def toString = s"${conjunct}\n${Tabulator.tuplesToTable(tuples)}"
}

case object ConjunctTable {
    
  def conjunctProjection(c: TConjunct): Tuple => Tuple =
    (t: Tuple) => {
      val attrs = t.attrs.keys.filter(a => a.startsWith(s"${c.name}."))
      Conditions.On(attrs.toSeq:_*)(t)
    }    
  
  def factorExecTuple(et: ExecTuple): List[ConjunctTable] = {
    val conjuncts = et.query.conjuncts
    val tuple = et.tuple
    for (c <- conjuncts; f = conjunctProjection(c)) yield ConjunctTable(c, List(f(tuple)))
  }
  
  def factorExecTuples(execTuples: Iterable[ExecTuple]): List[ConjunctTable] = {
    val unitTables = execTuples.flatMap(factorExecTuple)
    val groupedTables = unitTables.groupBy(_.conjunct)
    val mergedTables = for ((c, tbls) <- groupedTables; 
    						tuples = tbls.flatMap(_.tuples).toList.distinct) 
    					yield ConjunctTable(c, tuples)
    val results = mergedTables.toList
    results
  }

}

object MyTest extends App {
  val qa = QASystem.getInstance((new QAConfig()).copy(paraphraser = "templatesLm")).get
  val groups = qa.answer("What ingredients are in Indian cuisine?")
  for (group <- groups) {
    println(group.answerString)
    println(group.score)
    for (deriv <- group.invertedDerivations) {
      println(s"\tFrom Query: ${deriv.execQuery}")
      println(s"\tFrom Parsed Queries: ${deriv.parsedQueries.mkString(", ")}")
      println(s"\tFrom Paraphrases: ${deriv.paraphrases.map(_.target).mkString(", ")}")
      println(s"\tEvidence:")
      for (tbl <- deriv.tables) {
        println(s"\t${tbl.conjunct}")
        println(Tabulator.tuplesToTable(tbl.tuples))
      }
      println("-"*80)
    }
  }
    
}