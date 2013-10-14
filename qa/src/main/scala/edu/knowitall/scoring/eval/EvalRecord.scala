package edu.knowitall.scoring.eval

import edu.knowitall.apps.QAConfig

/**
 *  Represents a line from the labeled answers file.
 *  Contains information for linking an answer back to the configuration that produced it,
 *  so that the system does not need to be run a second time in order to produce scoring information.
 */
case class InputRecord(
    label: Option[String],
    answer: Option[String],
    scoreString: Option[String],
    just: Option[String],
    query: Option[String],
    paraphrase: String,
    question: String,
    parser: String,
    executor: String,
    grouper: String,
    scorer: String,
    json: Option[String]) {

  def sysConfig = QAConfig("TODO", parser, executor, grouper, scorer)

  override def toString: String = {
    val fields: Seq[String] = Seq(
      label.getOrElse("X"),
      answer.getOrElse("X"),
      question,
      just.getOrElse("X"),
      query.getOrElse("X"),
      paraphrase,
      scoreString.getOrElse("X"),
      parser,
      executor,
      grouper,
      scorer,
      json.getOrElse("X")
    )
    fields.mkString("\t")
  }
}

object InputRecord {

  import edu.knowitall.scoring.ScoredAnswerGroup
  import org.apache.commons.codec.binary.Base64
  import java.io.ByteArrayInputStream
  import java.io.ObjectInputStream

  private val b64 = new Base64()

  /**
   * In order of relevance to someone doing labeling.
   */
  val headers = Seq(
    "Label",
    "Answer",
    "Question",
    "Justification",
    "Query",
    "Paraphrase",
    "Score",
    "Parser",
    "Executor",
    "Grouper",
    "Scorer",
    "Json"
  ).mkString("\t")

  private def nx(str: String) = str match {
    case "X" => None
    case _ => Some(str)
  }

  def fromString(str: String): InputRecord = str.split("\t") match {
    // wrap plain file of questions in empty input record
    case Array(question) => InputRecord(None, None, None, None, None, question, question, "NA", "NA", "NA", "NA", None)
    case Array(label, answer, question, just, query, pphrase, score, parser, exec, grouper, scorer, json) =>

      InputRecord(nx(label), nx(answer), nx(score), nx(just), nx(query), pphrase, question, parser, exec, grouper, scorer, nx(json))
    case _ => throw new RuntimeException("Don't know how to parse:\n"+str)
  }

  def b64deserializeSag(string: String): ScoredAnswerGroup = {
    val yourBytes = b64.decode(string)
    val bis = new ByteArrayInputStream(yourBytes)
    val ois = new ObjectInputStream(bis)
    val sag = ois.readObject().asInstanceOf[ScoredAnswerGroup]
    bis.close()
    ois.close()
    sag
  }
}
