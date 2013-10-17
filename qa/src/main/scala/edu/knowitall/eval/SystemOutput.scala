package edu.knowitall.eval

import scala.io.Source
import java.io.File
import com.typesafe.config.ConfigFactory

abstract class SystemOutput {
  
  def path: String
  
  def normalize = Oracle.normalize _
  
  def recordsFor(input: String, output: String): List[OutputRecord] = {
    val in = normalize(input)
    val on = normalize(output)
    inputOutputToRecords.getOrElse((in, on), List()).toList
  }

  private val inputToRecords = records.groupBy(r => normalize(r.input))

  private val inputOutputToRecords = records.groupBy(r => (normalize(r.input), normalize(r.output)))
  
  def topOutputFor(input: String): Option[String] = {
    inputToRecords.get(normalize(input)) match {
      case Some(l) => Some(l.maxBy(_.score).output)
      case _ => None
    }
  }
  
  def records: List[OutputRecord]

}

case object SystemOutput {
  val conf = ConfigFactory.load()
  val outputFile = conf.getString("eval.output.file")

  private case class SystemOutputImpl(path: String, records: List[OutputRecord]) extends SystemOutput {
    def apply(path: String, records: List[OutputRecord]) = SystemOutputImpl(path, records)    
  }
  def loadRecords(path: String): List[OutputRecord] = {
    val lines = Source.fromFile(new File(path, outputFile), "UTF8").getLines
    lines.map(OutputRecord.fromLine).toList
  }
  def fromPath(path: String): SystemOutput = SystemOutputImpl(path, loadRecords(path))
}