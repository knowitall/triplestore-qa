package edu.knowitall.eval.paraphrasing

import edu.knowitall.apps.QASystem
import edu.knowitall.paraphrasing.Paraphraser
import scala.io.Source
import org.slf4j.LoggerFactory
import edu.knowitall.eval.OutputRecord
import edu.knowitall.eval.SystemOutput

class ParaphraseSystemRunner(paraphraser: Paraphraser, path: String) {
  
  val logger = LoggerFactory.getLogger(this.getClass)

  
  def runFile(path: String) = {
	val lines = Source.fromFile(path, "UTF8").getLines.toList
	
  }
  
  def run(name: String, inputs: List[String]) = {
    val n = inputs.size
    val records = for ((q, i) <- inputs.zipWithIndex;
    				   pp <- {
    				     logger.info(s"Question ${i+1} of ${n}")
    				     paraphraser.paraphrase(q)
    				   };
    				   r = OutputRecord(q, pp.target, pp.derivation.score))
    				yield r
    val output = SystemOutput(path, records.toList)
    output.save
  }

}

object ParaphraseSystemRunner extends App {
  val input = args(0)
  val output = args(1)
  val pp = QASystem.getInstance().get.paraphraser
  val runner = new ParaphraseSystemRunner(pp, output)
  runner.runFile(input)
}