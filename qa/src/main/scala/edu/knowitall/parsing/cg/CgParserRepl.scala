package edu.knowitall.parsing.cg

class CgParserRepl {
  val parser = new CgParser()
  def eval(input: String) = {
    val parses = parser(input)
	val lines = for {
	  parse <- parses
	  qstr = parse.query.toString
	  terms = parse.derivation.terminals.mkString("\n")
	} yield s"$qstr\n$terms\n"
	lines.mkString("\n")
  }
}

object CgParserRepl extends App {
  import jline.console.ConsoleReader
  val reader = new ConsoleReader()
  val repl = new CgParserRepl
  while (true) {
	val line = reader.readLine("> ")
	println(repl.eval(line))
  }
}