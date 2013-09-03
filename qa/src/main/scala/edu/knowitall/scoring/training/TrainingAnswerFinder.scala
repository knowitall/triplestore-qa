package edu.knowitall.scoring.training

import edu.knowitall.apps.QAConfig
import edu.knowitall.apps.QASystem
import java.io.PrintStream
import java.io.File
import scopt.OptionParser
import edu.knowitall.common.Resource.using

object TrainingAnswerFinder {

  case class InputQuestion(englishQuestion: String, formalQuestion: String)
  
  case class OutputQuestion(answer: String, inputQ: InputQuestion)
  
  case class Config(
      inputFile: File = new File("."), 
      output: PrintStream = System.out, 
      maxAnswersPerQ: Int = 20,
      parserName: String = "formal", 
      executor: String = "identity", 
      grouper: String = "basic",
      scorer: String = "numDerivations")
  
  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("TrainingAnswerFinder") {
      arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
      opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f, "UTF8")) }
      opt[Int]("maxAnswersPerQ") action { (f, c) => c.copy(maxAnswersPerQ = f) }
      opt[String]("parser") action { (f, c) => c.copy(parserName = f) }
      opt[String]("executor") action { (f, c) => c.copy(executor = f) }
      opt[String]("grouper") action { (f, c) => c.copy(grouper = f) }
      opt[String]("scorer") action { (f, c) => c.copy(scorer = f) }
    }
    
    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case _ => 
    } 
  }
  
  def run(config: Config): Unit = {

    val sysConfig = QAConfig(config.parserName, config.executor, config.grouper, config.scorer)
    val system = QASystem.getInstance(sysConfig).get

    using(io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      
      val lines = source.getLines
      val inputQs = lines.map(_.split("\t")).flatMap {
        case Array(english, formal) => Some(InputQuestion(english, formal))
        case x: Array[String] => 
          System.err.println("Warning, malformed training question: " + x.mkString("\t")); 
          None
      }
      
      val outputQs = inputQs.flatMap { q =>
        val answerGroups = system.answer(q.formalQuestion)
        val answers = answerGroups.map(_.alternates.head.head)
        val topAnswers = answers.take(config.maxAnswersPerQ)
        topAnswers.map(a => OutputQuestion(a, q))
      }
      
      outputQs.toList.foreach { oq => 
        val outFields = Seq("X", oq.answer, oq.inputQ.formalQuestion, oq.inputQ.englishQuestion)
        val outString = outFields.mkString("\t")
        config.output.println(outString)
      }
    }
  }
}