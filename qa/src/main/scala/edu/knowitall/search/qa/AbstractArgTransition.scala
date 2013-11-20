package edu.knowitall.search.qa

import edu.knowitall.tool.tokenize.Tokenizer
import com.typesafe.config.ConfigFactory
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.search.Transition
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer

class AbstractArgTransition(
    tokenizer: Tokenizer = AbstractArgTransition.defaultTokenizer,
    stemmer: Stemmer = AbstractArgTransition.defaultStemmer,
    tagger: Postagger = AbstractArgTransition.defaultPostagger,
    maxArgLength: Int = AbstractArgTransition.defaultMaxArgLength, 
    multipleParaphrases: Boolean = AbstractArgTransition.multipleParaphrases)
    extends Transition[QaState, QaAction] {
  
  private final val action = AbstractArgAction() 
  
  override def apply(s: QaState) = s match {
    case s: QuestionState => abstractArgs(s)
    case _ => Nil
  }
  
  private def intervals(size: Int) =
    for (i <- Range(0, size); j <- Range(i, size); if j+1-i <= maxArgLength) 
      yield Interval.open(i, j+1)
      
  private def stemString(s: String): Seq[String] = {
    val tokens = tokenizer(s)
    val tagged = tagger.postagTokens(tokens)
    tagged.map {
      t => MorphaStemmer.lemmatizePostaggedToken(t).lemma.toLowerCase() 
    }
  }
  
  private def abstractArgs(s: QuestionState) = 
    if (multipleParaphrases || !s.isParaphrased) {
      val toks = stemString(s.question).toIndexedSeq
      for {
        interval <- intervals(toks.size)
        newState = AbstractedArgState(s.question, toks, interval)
      } yield (action, newState)
    } else {
      Nil
    }
  
      
}

case class AbstractArgAction() extends QaAction

case object AbstractArgTransition {
  val conf = ConfigFactory.load()
  val defaultMaxArgLength = conf.getInt("paraphrase.template.maxArgLength")
  val multipleParaphrases = conf.getBoolean("paraphrase.template.multipleParaphrases")
  lazy val defaultTokenizer = new ClearTokenizer
  lazy val defaultPostagger = new StanfordPostagger
  lazy val defaultStemmer = new MorphaStemmer
}