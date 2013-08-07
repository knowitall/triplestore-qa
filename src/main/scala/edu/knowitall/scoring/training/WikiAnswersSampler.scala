package edu.knowitall.scoring.training

/**
 * A utility (throw-away?) for sampling answers.
 */
class WikiAnswersSampler(val inputFile: String) extends Iterable[String] {

  import io.Source
  
  /**
   * Get the median-length question.
   */
  def processLine(line: String): String = {
    
    val parts = line.split("\t")
    val questions = parts.filter(_.startsWith("q:"))
    val cleaned = questions.map(_.drop(2).trim)
    val lengthSorted = cleaned.sortBy(_.length)
    val midIndex = lengthSorted.length / 2
    lengthSorted(midIndex)
  }
  
  def iterator = new Iterator[String]() {
    val source = io.Source.fromFile(inputFile, "UTF8")
    val lines = source.getLines
    var closed = false
    def hasNext = {
      if (closed) false
      else if (!lines.hasNext) {
        source.close
        closed = true
        false
      }
      else true
    }
    def next = processLine(lines.next)
  }
}

object WikiAnswersSampler {
  
  import edu.knowitall.common.Resource.using
  import java.io.PrintStream
  
  def main(args: Array[String]): Unit = {

    val inputFile = args(0)
    val outputFile = args(1)

    val waSampler = new WikiAnswersSampler(inputFile)

    using(new PrintStream(outputFile, "UTF8")) { output =>
      waSampler foreach output.println
    }
  }
}