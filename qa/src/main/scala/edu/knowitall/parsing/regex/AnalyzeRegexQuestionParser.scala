package edu.knowitall.parsing.regex

object AnalyzeRegexQuestionParser2 extends App {

  import RegexQuestionPatterns.patterns
  import RegexQuestionPatterns._
  import edu.knowitall.util.WikiAnswersSampler

  val random = new scala.util.Random(0)

  val parser = new RegexQuestionParser()

  val wikiSampler = new WikiAnswersSampler(args(0))

  val pronoun = "<pos='PRP' | pos='PRP$'>"
  val classInst = "<lemma='type' | lemma='kind' | lemma='class' | lemma='sort'>"
  val whatWhich = "<lemma='what' | lemma='which'>"
  val wordNoPOS =     "< pos='CD' | pos='DT' | pos='NN' " +
      "| pos='NNS' | pos='NNP' | pos='NNPS' " +
      "| pos='VBN' | pos='VBG'>";
  val entNoPOS = s"(?:$modifier* $wordNoPOS+)+"

  // What be the $NUM $ents?
  val testPattern =    new RegexQuestionPattern(
      Seq("ent"),
      s"$whatWhich <lemma='be'> <pos='DT'>? <pos='CD'> (?:$word+ $prep)? (<ent>:$ent) $punct?",
      "$x: ($x, " + isa + ", $ent)")

  val wikiQs = random.shuffle(wikiSampler.flatten).toSeq.take(1000)
  val answerableQs = io.Source.fromFile("../training-500-answerable.txt", "UTF8").getLines.toSeq
  printParses(testPattern, wikiQs)
  countParses(testPattern, wikiQs)
  println()
  printParses(testPattern, answerableQs)
  countParses(testPattern, answerableQs)

  //printAllQuestionParses(answerableQs)


  def printAllQuestionParses(questions: Seq[String]): Unit = {
    val lemmas = questions.map(parser.lemmatize)
    val allParses = questions.zip(lemmas).foreach { case (question, lemma) =>

      val parses = parser.parse(lemma)
      if (parses.isEmpty) println(question)
      else parses.foreach { p => println(question + "\t" + p) }
    }
  }

  def countParses(pattern: RegexQuestionPattern, questions: Seq[String]): Unit = {
    val lemmas = questions.map(parser.lemmatize)

    val patternparses = lemmas.map(pattern.parse)

    val allparses = lemmas.map(parser.parse)

    println(patternparses.filter(_.nonEmpty).size)

    println(allparses.filter(_.nonEmpty).size)

    println(questions.size)
  }

  def printParses(pattern: RegexQuestionPattern, questions: Seq[String]): Unit = {
    val lemmas = questions.iterator map parser.lemmatize

    val parses = lemmas.flatMap { lemma =>
      pattern.parse(lemma).take(1).map((lemma, _))
    }
    parses foreach { case (lemma, parse) =>
      println(lemma.map(_.string).mkString(" ") + "\t" + parse)
    }
  }
}