package edu.knowitall.execution

case class StopwordExecutor(baseExecutor: QueryExecutor) extends QueryExecutor {
  
  import java.util.regex.Pattern
  
  val stops = Set("a", "an", "the", "'s")
  
  type ADs = Iterable[AnswerDerivation]
  
  def deriveAnswers(q: UQuery): ADs = q match {
    case c: ListConjunctiveQuery => baseExecutor.deriveAnswers(cleanQuery(c))
    case _ => throw new 
      UnsupportedOperationException(s"Unable to execute query type: $q")
  }
  
  def cleanQuery(q: ListConjunctiveQuery): ListConjunctiveQuery = {
    q.copy(conjuncts = q.conjuncts map cleanConjunct)
  }

  def cleanConjunct(c: TConjunct): TConjunct = {
    val cleanValues = c.values.map {
      case (key, UnquotedTLiteral(value)) =>
        (key, UnquotedTLiteral(cleanWords(value)))
      case (key, value) => (key, value)
    }
    c.copy(values = cleanValues)
  }
  
  def cleanWords(str: String) = {
    val cleaned = str.split(" ").filterNot(stops.contains).mkString(" ")
    if (cleaned.isEmpty) str else cleaned
  }
}