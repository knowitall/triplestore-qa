package edu.knowitall.execution

import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.apps.QASystem

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
  val qa = QASystem.getInstance().get
  val groups = qa.answer("Which founding fathers owned slaves?")
  for (group <- groups; 
       derivs = group.derivations;
       groupedDerivs = derivs.groupBy(d => d.execTuple.query);
       (query, subDerivs) <- groupedDerivs;
       x = println("-----------------------------------\n" + query);
       subTuples = subDerivs.map(d => d.execTuple);
       tables = ConjunctTable.factorExecTuples(subTuples);
       table <- tables;
       y = println(table.toString + "\n")) {}  
    
}