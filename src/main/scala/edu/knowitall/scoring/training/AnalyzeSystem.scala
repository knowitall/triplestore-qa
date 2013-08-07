package edu.knowitall.scoring.training

import edu.knowitall.apps.QASystem
import edu.knowitall.apps.QAConfig

class AnalyzeSystem(val input: Seq[QAPair], val system: QASystem) {
  
  def scorePairs(question: String, pairs: Seq[QAPair]): Scored[QAPair] = {
   
    val answerScores: Map[String, Double] = system.answer(question).map(g => (g.alternates.head.head, g.score)).toMap
    val existingScores = pairs.map(_.answer).toSet
    (answerScores.keySet &~  existingScores).foreach { ans => System.err.println("Warning, found answer not in eval set: $ans")}
    
    val scoredPairs = pairs.flatMap { p =>
      val scoreLookup = answerScores.get(p.answer)
      scoreLookup match {
        case Some(score) => Some(Scored(p, score))
        case None => {
          //System.err.println(s"Warning, no score found for $p")
          None
        }
      }
    }
    scoredPairs.maxBy(_.score)
  }
  
  lazy val output: Seq[Scored[QAPair]] = {
    
    // don't bother with unlabeled questions
    val allLabeled = input.filter(p => p.label == "1" || p.label == "0")
    
    val questionPairs = allLabeled.groupBy(_.question)
    questionPairs.iterator.toSeq.map { case (question, pairs) => scorePairs(question, pairs) }
  }
  
  def precision: Seq[Double] = {
    val booleans = output.sortBy(-_.score).map(_.item.label == "1")
    AnalyzeClassifier.precRecall(booleans)
  }
}

object AnalyzeSystem extends App {
  
  import scopt.OptionParser
  import java.io.{File, PrintStream}
  import edu.knowitall.common.Resource.using
  
  case class Config(inputFile: File = new File("."), output: PrintStream = System.out)
  
  val parser = new OptionParser[Config]("EvaluationAnswerFinder") {
    arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
    opt[File]("outputFile") action { (f, c) => c.copy(output = new PrintStream(f)) }
  }
  
  parser.parse(args, Config()).foreach { config =>
    run(config)
  }
  
  def run(config: Config): Unit = {
    
    val input = using(io.Source.fromFile(config.inputFile, "UTF8")) { source =>
      source.getLines.map(QAPair.deserialize).toList  
    }
    
    val sysConfig = QAConfig("regex pattern", "identity", "basic", "logistic")
    val system = QASystem.getInstance(sysConfig).get
    
    val analyzer = new AnalyzeSystem(input, system)
    
    val output = analyzer.precision.zipWithIndex.map {
      case (prec, index) => s"$index\t$prec"
    }
    using(config.output) { outStream => output.foreach(outStream.println) }
  }
}