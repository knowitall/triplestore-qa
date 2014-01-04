package edu.knowitall.paraphrasing.template

import edu.knowitall.util.MathUtils
import com.clearspring.analytics.stream.frequency.CountMinSketch
import scala.io.Source
import java.io.FileInputStream
import java.io.File
import java.io.InputStream
import java.io.FileOutputStream
import java.nio.file.Files
import java.nio.file.Paths

case class StemmedQuestion(tokens: Seq[String]) {
  def this(s: String) = this(s.trim.split(" +"))
  
  def templateArgPairs = { for {
      i <- MathUtils.allIntervals(tokens.size - 1)
      if i.size != tokens.size && i.size > 0
      t = tokens.slice(0, i.start) ++ Seq("$y") ++ tokens.slice(i.end, tokens.size)
      a = tokens.slice(i.start, i.end)
    } yield List(t.mkString(" "), a.mkString(" ")) 
  } map {
    case List(t, a) => (t, a)
  }
  
  def templateArgStrings = { templateArgPairs.map { 
    case (a, b) => List(a, b)
  }}.flatten
}

case object StemmedQuestion {
  def fromFile(path: String) = fromInputStream(new FileInputStream(new File(path)))
  def fromInputStream(is: InputStream) = Source.fromInputStream(is, "UTF-8").getLines.map(new StemmedQuestion(_)).toIterable
}

case class Counter(err: Double = 1e-7, prob: Double = 0.9, seed: Int = 123) {
  def sketchQuestions(lines: Iterable[String]) = {
    val chunkSize = 128 * 1024
    val argSketch = new CountMinSketch(err, prob, seed)
    val templateSketch = new CountMinSketch(err, prob, seed)
    val iterator = lines.grouped(chunkSize)
    iterator.foreach { lines =>
      lines.par.foreach {
        line => {
          val sq = new StemmedQuestion(line)
          for ((t, a) <- sq.templateArgPairs) {
            argSketch.add(a, 1)
            templateSketch.add(t, 1)
          }
        }
      }
    }
    (templateSketch, argSketch)
  }
}

case object Counter extends App {
  val sqs = StemmedQuestion.fromInputStream(System.in)
  val outFile = args(0)
  val counter = Counter()
  val (templateSketch, argSketch) = counter.sketchQuestions(Source.fromInputStream(System.in, "UTF-8").getLines.toIterable)
  val argBytes = CountMinSketch.serialize(argSketch)
  val templateBytes = CountMinSketch.serialize(templateSketch)
  val argOut = new FileOutputStream(args(0) + ".arg")
  val templateOut = new FileOutputStream(args(0) + ".temp")
  argOut.write(argBytes)
  argOut.close()
  templateOut.write(templateBytes)
  templateOut.close()
}

case object EnumeratePatterns extends App {
  val argSketch = CountMinSketch.deserialize(Files.readAllBytes(Paths.get(args(0) + ".arg")))
  val templateSketch = CountMinSketch.deserialize(Files.readAllBytes(Paths.get(args(0) + ".temp")))
  val iterator = Source.fromInputStream(System.in, "UTF-8").getLines.grouped(128 * 1024) 
  for (lines <- iterator; line <- lines.par) {
    val sq = new StemmedQuestion(line)
    for {
      (t, a) <- sq.templateArgPairs
      ct = templateSketch.estimateCount(t)
      ca = argSketch.estimateCount(a)
      if ct > 50 && ca > 50
    } println(s"${t.replaceAll(" ", "_")}\t${a.replaceAll(" ", "_")}\t$ct\t$ca")
  }
}