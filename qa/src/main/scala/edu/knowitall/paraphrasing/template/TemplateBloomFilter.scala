package edu.knowitall.paraphrasing.template

import com.typesafe.config.ConfigFactory
import com.clearspring.analytics.stream.membership.BloomFilter
import edu.knowitall.util.ResourceUtils
import org.apache.commons.io.IOUtils
import java.io.FileOutputStream
import scala.io.Source

object TemplateBloomFilter {
  val conf = ConfigFactory.load()
  val defaultNumTemplates = conf.getInt("paraphrase.template.numTemplates")
  val defaultProbFalsePos = conf.getDouble("paraphrase.template.probFalsePos")
  val filterPath = conf.getString("paraphrase.template.bloomFilterPath")
  lazy val defaultFilter = readFilter(filterPath)
  
  def createFilter(templates: Iterable[String], 
      numTemplates: Int = TemplateBloomFilter.defaultNumTemplates,
      probFalsePos: Double = defaultProbFalsePos) = {
    val bf = new BloomFilter(numTemplates, probFalsePos)
    for (t <- templates) bf.add(t)
    bf
  } 
  
  def readFilter(path: String = filterPath) = {
    val in = ResourceUtils.resource(filterPath)
    val bytes = IOUtils.toByteArray(in)
    BloomFilter.deserialize(bytes)
  }
  
  def writeFilter(bf: BloomFilter, path: String = filterPath) = {
    val out = new FileOutputStream(path)
    out.write(BloomFilter.serialize(bf))
    out.close()
  }
  
}

object MakeTemplateBloomFilter extends App {
  override def main(args: Array[String]) = {
    val out = args(0)
    val lines = Source.fromInputStream(System.in, "UTF-8").getLines.toIterable
    val bf = TemplateBloomFilter.createFilter(lines)
    println(s"num buckets = ${bf.buckets}, num hashes = ${bf.getHashCount}")
    TemplateBloomFilter.writeFilter(bf, out)
  }
}