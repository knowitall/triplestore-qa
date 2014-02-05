package edu.knowitall.wikianswers

import scala.io.Source

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