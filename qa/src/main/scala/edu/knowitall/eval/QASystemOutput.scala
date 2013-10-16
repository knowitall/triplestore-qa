package edu.knowitall.eval

import com.typesafe.config.ConfigFactory
import java.io.File
import java.io.PrintWriter
import scala.collection.JavaConverters._
import scala.io.Source
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.apps.AnswerDerivation
import com.typesafe.config.ConfigRenderOptions
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.Search

case class SystemOutputRecord(question: String, answer: String, score: Double, derivation: String) {
  override def toString = s"$question\t$answer\t$score\t$derivation"
}
case object SystemOutputRecord {
  def fromLine(line: String) = line.split("\t") match {
    case Array(q, a, s, d) => SystemOutputRecord(q, a, s.toDouble, d)
    case _ => throw new IllegalArgumentException(s"Could not parse line: $line")
  }
  def fromScoredAnswerGroup(question: String, group: ScoredAnswerGroup) = {
    val answer = group.answerString
    val derivs = group.derivations.map(derivationToString).mkString("; ")
    SystemOutputRecord(question, answer, group.score, derivs)
  }
  def derivationToString(d: AnswerDerivation) = {
    val answer = d.answerString
    val para = d.paraphrase.target
    val query = d.parsedQuery.toString
    val equery = d.execTuple.query.toString
    val tuple = project(d.execTuple.tuple)
    s"$para => $query => $equery => $tuple => $answer"
  }
  def project(t: Tuple): String = {
    val tup = Search.ProjectTriples(List(t)).toList(0)
    tup.toString
  }
}

case class QASystemOutput(path: String, records: List[SystemOutputRecord], config: Map[String, String], name: String) {
  
  def this(path: String, name: String) = this(path, QASystemOutput.loadRecords(path), QASystemOutput.loadConfig(path), name)
  def this(path: String, records: List[SystemOutputRecord], name: String) = this(path, records, QASystemOutput.getCurrentConfig, name)
  
  def normalize = QAOracle.normalize _

  val questions = records.map(_.question).distinct
  val questionAnswers = records.map(r => (r.question, r.answer)).distinct
  
  private val questionToRecords = records.groupBy(r => normalize(r.question))
  private val questionAnswerToRecords = records.groupBy(r => (normalize(r.question), normalize(r.answer)))
  
  def save = {
    
    val dir = new File(path)
    if (dir.exists() && !dir.isDirectory()) throw new IllegalStateException(s"$dir exists but is not a directory")
    if (!dir.exists()) dir.mkdirs()
    
    val outputPath = new File(path, QASystemOutput.outputFile)
    val outputWriter = new PrintWriter(outputPath)
    records.foreach(outputWriter.println(_))
    outputWriter.close()
    
    val configPath = new File(path, QASystemOutput.configFile)
    val configWriter = new PrintWriter(configPath)
    for ((k, v) <- config) configWriter.println(s"$k\t$v")
    configWriter.close()
    
    val namePath = new File(path, QASystemOutput.nameFile)
    val nameWriter = new PrintWriter(namePath)
    nameWriter.println(name)
    nameWriter.close()
     
  }
  
  def recordsFor(q: String, a: String) = {
    val qn = normalize(q)
    val an = normalize(a)
    questionAnswerToRecords.getOrElse((qn, an), List())
  }
  
  def topAnswerFor(q: String): Option[String] = {
    questionToRecords.get(normalize(q)) match {
      case Some(l) => Some(l.maxBy(_.score).answer)
      case _ => None
    }
  }

}

case object QASystemOutput {
  val conf = ConfigFactory.load()
  val outputFile = conf.getString("eval.output.file")
  val configFile = conf.getString("eval.config.file")
  val nameFile = conf.getString("eval.name.file")
  def loadRecords(path: String) = {
    val lines = Source.fromFile(new File(path, outputFile)).getLines
    lines.map(SystemOutputRecord.fromLine).toList
  }
  def loadConfig(path: String) = {
    val lines = Source.fromFile(new File(path, configFile)).getLines
    val pairs = lines map { line: String =>
      line.split("\t", 2) match {
        case Array(k, v) => (k, v)
        case _ => throw new IllegalArgumentException(s"Could not parse line: $line")
      }
    }
    pairs.toMap
  }
  def getCurrentConfig = conf.root().asScala.map(pair => (pair._1, pair._2.render(ConfigRenderOptions.concise))).toMap
  def fromPath(path: String) = {
    val name = Source.fromFile(new File(path, nameFile)).getLines.mkString("\n")
    new QASystemOutput(path, name)
  }
}