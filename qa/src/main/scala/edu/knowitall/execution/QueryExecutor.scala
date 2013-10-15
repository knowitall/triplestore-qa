package edu.knowitall.execution
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.rel
import edu.knowitall.triplestore.TriplestoreClient
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.paraphrasing.Paraphrase

case class ExecTuple(tuple: Tuple, query: ConjunctiveQuery) {
  val answer: List[String] = query.qAttrs.flatMap(a => tuple.getString(a))
  val answerString: String = answer match {
    case List(a) => a
    case _ => "(" + answer.mkString(", ") + ")"
  }
}

trait QueryExecutor {
  def execute(query: ConjunctiveQuery): Iterable[ExecTuple]
}

case class IdentityExecutor(client: TriplestoreClient) extends QueryExecutor {

  val logger = LoggerFactory.getLogger(this.getClass)

  val joiner = Joiner(client)

  override def execute(q: ConjunctiveQuery): Iterable[ExecTuple] = {
    for (t <- joiner.joinQueries(q.conjuncts);
         et = ExecTuple(t, q)) yield et
  }
}