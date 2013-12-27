package edu.knowitall.parsing.cg

import com.typesafe.config.ConfigFactory
import scala.io.Source
import edu.knowitall.util.ResourceUtils

case class LexiconPreprocessor(macros: Map[String, String] = LexiconPreprocessor.defaultMacros) {
  val mlist = macros.toList
  def apply(s: String) = applyMacros(mlist, s)
  private def applyMacros(nr: List[(String, String)], s: String): String = nr match {
    case Nil => s
    case (name, replacement) :: rest => applyMacros(rest, applyMacro(name, replacement, s))
  }
  private def applyMacro(name: String, replacement: String, s: String) = {
    s.replaceAll(s"@${name}", replacement)
  }
}

object LexiconPreprocessor {
  val conf = ConfigFactory.load()
  
  def fromLines(lines: Iterable[String]) = {
    val pairs = for {
      line <- lines
      linet = line.trim
      if !linet.startsWith("#") && !line.startsWith("//")
      (name, value) <- linet.split(" ", 2) match {
        case Array(n, v) => Some((n, v))
        case _ => None
      }
    } yield (name, value)
    pairs.toMap
  }
  
  lazy val defaultMacros = if (conf.hasPath("parsing.cg.macroPath")) {
    val lines = Source.fromFile(conf.getString("parsing.cg.macroPath"), "UTF-8").getLines
    fromLines(lines.toIterable)
  } else if (conf.hasPath("parsing.cg.macroClasspath")) {
    val p = conf.getString("parsing.cg.macroClasspath")
    val lines = Source.fromInputStream(ResourceUtils.resource(p), "UTF-8").getLines
    fromLines(lines.toIterable)
  } else {
    Map.empty[String, String]
  }
}