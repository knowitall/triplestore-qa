package edu.knowitall.wikianswers

import scala.io.Source
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.Scoobi.ScoobiApp

object QuestionCategories extends App {
  
  val lines = Source.fromInputStream(System.in, "UTF-8").getLines
  
  for {
    line <- lines
    doc <- WikiAnswersDoc.lineToDoc(line)
    if doc.docType == WikiAnswersDoc.QUESTION_DOC
    q <- WikiAnswersDoc.getTitle(doc)
    cats = WikiAnswersDoc.getCategories(doc).mkString("\t")
    if cats.size > 1
  } println(s"$q\t$cats")
  

}

object QuestionCategoriesScoobi extends ScoobiApp {
  
  def outputRow(line: String) = for {
    doc <- WikiAnswersDoc.lineToDoc(line)
    if doc.docType == WikiAnswersDoc.QUESTION_DOC
    q <- WikiAnswersDoc.getTitle(doc)
    cats = WikiAnswersDoc.getCategories(doc).mkString("\t")
    if cats.size > 0
  } yield s"$q\t$cats"
  
  def run() {
    val lines = ScoobiUtils.textFromLzo(args(0))
    val output = lines.mapFlatten(outputRow)
    persist(output.toTextFile(args(1), true))
  }
  
}