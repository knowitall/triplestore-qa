package edu.knowitall.scoring.training

import java.io.File
import java.io.PrintStream
import LabelAnnotator._
import edu.knowitall.tool.conf.Labelled
import AnswerAnnotator.OutputRecord
import edu.knowitall.common.Resource.using
import io.Source
import java.net.URL

/**
 * Merges labeled and unlabeled training data to aid in
 * extending existing training sets. For example,
 * if you have existing data from AnswerAnnotator that you've
 * labeled, and you want to extend that labeled data with
 * a new set of unlabeled data, this class will merge and
 * deduplicate the two sets, preserving existing labels.
 * 
 * Input: Labeled File and Unlabeled File
 * Output: Mixed (Labeled/Unlabeled) - columns:
 * (label |)? answer | query | question 
 */
class LabelAnnotator(val labeled: Seq[LabeledRecord], val unlabeled: Seq[OutputRecord]) {
  
  def labeledToString(lr: LabeledRecord): String = {
    val labelString = if (lr.label) "1" else "0"
    Seq(labelString, lr.item.toString).mkString("\t")
  }
  
  def merged: Iterable[String] = {
    // group labeled and unlabeled by their OutputRecord, using toString as key.
    val labeledGroups = labeled.groupBy(_.item.toString)
    val unlabeledGroups = unlabeled.groupBy(_.toString)
    // delete unlabeled groups that exist in labeled groups
    // this yields only the new unlabeled stuff.
    val newUnlabeledGroups = unlabeledGroups -- labeledGroups.keySet
    val labeledStrings = labeledGroups.values.flatten.map(labeledToString)
    
    val newUnlabeled = newUnlabeledGroups.values.flatten.toSeq.sortBy(_.uQuery) 
    val unlabeledStrings = newUnlabeled.map(r => s"\t${r.toString}")
    labeledStrings ++ unlabeledStrings
  }
}

object LabelAnnotator {
  
  type LabeledRecord = Labelled[OutputRecord] 
  
  def loadLabeled(file: File): Seq[LabeledRecord] = using(Source.fromFile(file, "UTF8")) { source =>
    val lines = source.getLines
    loadLabeled(lines)
  }
  
  def loadLabeled(url: URL): Seq[LabeledRecord] = using(Source.fromURL(url, "UTF8")) { source =>
    val lines = source.getLines
    loadLabeled(lines)
  }
  
  def loadLabeled(lines: Iterator[String]): Seq[LabeledRecord] = {
    val labelRegex = """(0|1)\s+(.*)""".r
    val labelsRecords = lines.map {
      case labelRegex(label, record) => {
        require(label == "1" || label == "0")
        val boolLabel = label == "1"
        Labelled(boolLabel, OutputRecord.fromString(record))
      }
      case s => throw new RuntimeException("Can't parse labeledrecord from " + s)
    }
    labelsRecords.toList
  }

  def loadUnlabeled(file: File) = using(Source.fromFile(file, "UTF8")) { source =>
    val unlabeledRecords = source.getLines map OutputRecord.fromString
    unlabeledRecords.toList
  }
  
  def loadResource(str: String): URL = {
    val url = getClass.getResource(str)
    require(url != null, "Could not find " + str)
    url
  }
  
  def main(args: Array[String]): Unit = {
    
    val outputPath = args(0)
    
    val questionsPath = "scorer-questions.txt"
    val questionsUrl = loadResource(questionsPath)
    
    val labeledPath = "scorer-training.txt"
    val labeledUrl = loadResource(labeledPath)
    
    val newOutputRecords = using(Source.fromURL(questionsUrl, "UTF8")) { src => 
      val linesIterator = src.getLines
      val annotator = new AnswerAnnotator(linesIterator)
      annotator.outputRecords.toList
    }
    
    val labeled = loadLabeled(labeledUrl)
    
    val labelAnnotator = new LabelAnnotator(labeled, newOutputRecords)
    
    val output = new PrintStream(outputPath)
    
    labelAnnotator.merged foreach output.println
    
    output.close()
  }
}