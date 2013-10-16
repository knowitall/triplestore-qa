package edu.knowitall.eval

import com.typesafe.config.ConfigFactory
import java.io.File
import java.io.PrintWriter
import scala.collection.JavaConverters._
import scala.io.Source
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.apps.AnswerDerivation

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
    val tuple = d.execTuple.tuple.toString
    s"$answer <= $tuple <= $equery <= $query <= $para"
  }
}

case class QASystemOutput(path: String, records: List[SystemOutputRecord], config: Map[String, String]) {
  
  def this(path: String) = this(path, QASystemOutput.loadRecords(path), QASystemOutput.loadConfig(path))
  def this(path: String, records: List[SystemOutputRecord]) = this(path, records, QASystemOutput.getCurrentConfig)
  
  val answerMap = {
    val triples = records.map(r => (normalize(r.question), r.answer, r.score))
    val grouped = triples.groupBy(_._1)
    val mapped = for ((k, group) <- grouped; pairs = group.map(triple => (triple._2, triple._3)); pairMap = pairs.toMap) yield (k, pairMap)
    mapped.toMap
  }
  
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
     
  }
  
  def normalize = QAOracle.normalize _
  
  def answers(question: String): Map[String, Double] = answerMap.getOrElse(normalize(question), Map())
  
  def topAnswer(question: String): Option[String] = answerMap.get(normalize(question)) match {
    case Some(answers) if answers.size > 0 => Some(answers.keys.maxBy(answers.get))
    case _ => None
  }
}

case object QASystemOutput {
  val conf = ConfigFactory.load()
  val outputFile = conf.getString("eval.output.file")
  val configFile = conf.getString("eval.config.file")
  def loadRecords(path: String) = {
    val lines = Source.fromFile(new File(path, outputFile)).getLines
    lines.map(SystemOutputRecord.fromLine).toList
  }
  def loadConfig(path: String) = {
    val lines = Source.fromFile(new File(path, configFile)).getLines
    val pairs = lines map { line: String =>
      line.split("\t") match {
        case Array(k, v) => (k, v)
        case _ => throw new IllegalArgumentException("Could not parse line: $line")
      }
    }
    pairs.toMap
  }
  def getCurrentConfig = conf.root().asScala.map(pair => (pair._1, pair._2.toString)).toMap
}