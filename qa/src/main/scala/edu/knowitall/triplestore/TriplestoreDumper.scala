package edu.knowitall.triplestore

import org.apache.lucene.store.FSDirectory
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexableField
import scala.collection.JavaConversions._
import java.io.File

object TriplestoreDumper extends App {
  
  def fieldToPair(field: IndexableField) = (field.name, field.stringValue)
  
  override def main(args: Array[String]) {
    if (args.size != 3) {
      throw new IllegalArgumentException("Usage: class input-dir output-file")
    }
    val input = args(1)
    val output = args(2)
    val index = FSDirectory.open(new File(input))
    val reader = IndexReader.open(index)
    for {
      i <- 0 until reader.numDocs;
      doc = reader.document(i)
      fields = doc.getFields()
      pairs = fields.map(fieldToPair).map(x => x._1 + "\t" + x._2).mkString("\t")
    } println(pairs)
  }

}