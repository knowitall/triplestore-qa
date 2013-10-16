package edu.knowitall.eval

class AnswerLabeler(oracle: UpdateableQAOracle, output: QASystemOutput) {
  
  def generateTopLabelFile = {
    for (q <- output.questions; a <- output.topAnswerFor(q); if !oracle.hasLabel(q, a)) {
      println(s"LABEL\t0\t$q\t$a")
      val r = output.recordsFor(q, a).take(1)(0)
      println(r.derivation.split(";").take(1)(0).replaceAll(" => ", "\n=>\n"))
      println("\n")
    } 
  }
  
  def generateLabelFile = {
    for (r <- output.records; q = r.question; a = r.answer; if !oracle.hasLabel(q, a)) {
      println(s"LABEL\t$q\t$a\t0")
      println(r.derivation.split(";").take(1)(0).replaceAll(" => ", "\n=>\n"))
      println("\n")
    } 
  }

}

object AnswerLabeler extends App {
  val labelsPath = args(0)
  val outputPath = args(1)
  val oracle = new FileQAOracle(labelsPath)
  val output = QASystemOutput.fromPath(outputPath)
  val labeler = new AnswerLabeler(oracle, output)
  labeler.generateTopLabelFile
}