package edu.knowitall.parsing.cg

object CgParserRepl extends App {
  import sext._
  import jline.console.ConsoleReader
  val reader = new ConsoleReader()
  val parser = new CgParser()
  while (true) {
	val line = reader.readLine("> ")
	val parses = parser(line)
	val result = parses map { p => p.query.toString } mkString("\n")
   println(result)
  }
}