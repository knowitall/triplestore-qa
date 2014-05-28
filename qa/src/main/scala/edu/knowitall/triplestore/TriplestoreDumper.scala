package edu.knowitall.triplestore

import org.apache.lucene.store.FSDirectory
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexableField
import scala.collection.JavaConversions._
import java.io.File

object TriplestoreDumper extends App {
  
  def fieldToPair(field: IndexableField) = (field.name, field.stringValue)
  
   
  
  override def main(args: Array[String]) {
    if (args.size != 1) {
      throw new IllegalArgumentException("Usage: class input-dir")
    }
    val input = args(0)
    val output = args(1)	
    val index = FSDirectory.open(new File(input))
    val reader = DirectoryReader.open(index)
    
    for (i <- 1 until reader.numDocs) {
      val doc = reader.document(i)
      val fields = doc.getFields()
      val pairs = fields.map(fieldToPair)
      val line = pairs.map{ case (k, v) => k + "\t" + v }.mkString("\t")
      println(line)
    }
  }

}