package edu.knowitall.parsing.cg

object CgParserRepl extends App {
  import jline.console.ConsoleReader
  val reader = new ConsoleReader()
  val parser = new CgParser()
  while (true) {
	val line = reader.readLine("> ")
	val parses = parser(line)
	for (parse <- parses) {
	  println(parse.query)
	  parse.derivation.terminals foreach println
	  println
	}
  }
}