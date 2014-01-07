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
import edu.knowitall.wikianswers.QuestionCluster

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

case object TypeTemplate extends App {
  
  def isIsa(s: String) = s.toLowerCase == "instance of"
  
  val argTemplates = { for {
    line <- Source.fromFile(args(0), "UTF-8").getLines
    (arg, templates) <- line.split("\t").toList match {
      case arg :: rest => Some((arg, rest))
      case _ => None
    }
  } yield (arg, templates) }.toMap
  
  def lineToTriple(line: String) = line.trim.split("\t").toList match {
    case x :: r :: y :: Nil => Some((x.replaceAll(" ", "_"), r, y.replaceAll(" ", "_")))
    case _ => None
  }
  
  val iterator = Source.fromInputStream(System.in, "UTF-8").getLines.grouped(128*1024)
  val pairs = for {
    lines <- iterator
    line <- lines.par
    pairs = lineToTriple(line) match {
      case Some((arg1, rel, arg2)) if isIsa(rel) => for (t <- argTemplates.getOrElse(arg1, Set.empty)) yield s"$t\t$arg2"
      case _ => List.empty
    }
  } yield pairs
  pairs.flatten foreach println
}

case object TemplatePairExtractor extends App {
  def us2sp(s: String) = s.replaceAll("_", " ")
  def sp2us(s: String) = s.replaceAll(" ", "_")
  def loadSet(path: String) = Source.fromFile(path, "UTF-8").getLines.map(us2sp).toSet
  val arguments = loadSet(args(0))
  val templates = loadSet(args(1))
  val output = for {
    line <- Source.fromInputStream(System.in, "UTF-8").getLines
    cluster = QuestionCluster.fromString(line)
    counter = TemplateCounter(cluster)
    (tx1, tx2, a) <- counter.templatePairs
    (t1, t2) = if (tx1 < tx2) (tx1, tx2) else (tx2, tx1)
    if arguments contains a
    if templates contains t1
    if templates contains t2
  } yield s"${sp2us(t1)}\t${sp2us(t2)}\t${sp2us(a)}"
  output foreach println
}