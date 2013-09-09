package edu.knowitall.scoring

import java.io.File
import scopt.OptionParser
import edu.knowitall.common.Resource.using
import scala.util.Random
import scala.io.Source
import java.io.PrintStream

/**
 * Reads in paraphrase sets, one per line, consisting of tab-separated questions.
 * Randomly samples one question from each set.
 * Writes one question per line of output.
 */
object ParaphraseSampler {

  case class Config(inputFile: File = new File("."), outputFile: File = new File("."))
  
  private val randomSeed = 0
  
  private val random = new Random(randomSeed)
  
  private val tabRegex = "\t".r
  
  def main(args: Array[String]): Unit = {
    
    val parser = new OptionParser[Config]("ParaphraseSampler") {
      arg[File]("inputFile") action { (f, c) => c.copy(inputFile = f) }
      arg[File]("outputFile") action { (f, c) => c.copy(outputFile = f) }
    }
    
    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None =>
    }
  }
  
  def sampleParaphraseSet(pSet: String): String = {
    val split = tabRegex.split(pSet)
    val rIndex = random.nextInt(split.length)
    split(rIndex)
  }
  
  def run(config: Config): Unit = {
    using(Source.fromFile(config.inputFile, "UTF8")) { input =>
      using(new PrintStream(config.outputFile, "UTF8")) { output =>
        input.getLines map sampleParaphraseSet foreach output.println
      }
    }
  }
}