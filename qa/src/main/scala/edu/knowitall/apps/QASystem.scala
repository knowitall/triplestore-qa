package edu.knowitall.apps

import edu.knowitall.scoring.AnswerScorer
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.paralex.ParalexQuestionParser
import org.slf4j.LoggerFactory
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGrouper
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.execution.synonyms.LexiconSynonymExecutor
import edu.knowitall.execution.ClassInstanceExecutor
import edu.knowitall.execution.StopwordExecutor
import edu.knowitall.execution.generalize.DiffGeneralizingExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.BasicAnswerGrouper
import edu.knowitall.execution.PostagAnswerGrouper
import edu.knowitall.execution.PostagSampleAnswerGrouper
import edu.knowitall.execution.SingletonAnswerGrouper
import edu.knowitall.execution.ExactAnswerGrouper
import edu.knowitall.execution.UQuery
import edu.knowitall.scoring.NumDerivationsScorer
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.parsing.StringMatchingParser
import edu.knowitall.scoring.UniformAnswerScorer
import edu.knowitall.scoring.LogisticAnswerScorer
import edu.knowitall.parsing.OldParalexParser
import edu.knowitall.execution.synonyms.RelationSynonymExecutor
import edu.knowitall.common.Timing
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paralex.SolrParaphraseGenerator
import edu.knowitall.paralex.PmiLmScorer
import edu.knowitall.paralex.SimpleQuestionParaphraser
import com.typesafe.config.ConfigFactory
import edu.knowitall.paralex.QuestionParaphraser
import edu.knowitall.paralex.EmptyParaphraser

case class QASystem(paraphraser: QuestionParaphraser, parser: QuestionParser, 
    executor: QueryExecutor, grouper: AnswerGrouper, scorer: AnswerScorer) {

  val conf = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val maxDerivs = conf.getInt("qa.maxDerivs")
  val maxUQueries = conf.getInt("qa.maxUQueries")
  val maxParaphrases = conf.getInt("qa.maxParaphrases")
  val maxAnswerGroups = conf.getInt("qa.maxAnswerGroups")

  def answer(question: String): List[ScoredAnswerGroup] = {
    
    logger.info(s"Answering question '$question'")
    val paraphrases = Iterable(question) ++ paraphrase(question)
    
    logger.info(s"Paraphrased '$question' to:\n'${paraphrases.mkString("\n")}")
    val uqueries = parse(paraphrases)
    
    logger.info(s"Parsed '$question' into queries:\n'${uqueries.mkString("\n")}")
    val derivations = execute(uqueries)
    
    logger.info(s"Derived answers from '$question' to:\n'${derivations.map(_.answerString).mkString("\n")}")
    val answerGroups = group(derivations)
    
    logger.info(s"Grouped answers from '$question' to :\n'${answerGroups.map(_.answerString).mkString("\n")}")
    val scored = score(answerGroups).toList.sortBy(-_.score)
    
    logger.info(s"Scored answers from '$question' to :\n${scored.map(g => g.score + " " + g.answerString).mkString("\n")}")
    scored.take(maxAnswerGroups)
  }
  
  def paraphrase(question: String): Iterable[String] = 
    paraphraser.paraphrase(question).take(maxParaphrases)
    
  def parse(questions: Iterable[String]): Iterable[UQuery] = 
    questions.par.flatMap(parser.parse).toList.take(maxUQueries)
    
  def execute(uqueries: Iterable[UQuery]): Iterable[AnswerDerivation] =
    uqueries.par.flatMap(executor.deriveAnswers).toList.take(maxDerivs)
    
  def group(derivs: Iterable[AnswerDerivation]): Iterable[AnswerGroup] =
    grouper.group(derivs.toList)
    
  def score(groups: Iterable[AnswerGroup]): Iterable[ScoredAnswerGroup] =
    groups.par.map(scorer.scoreAnswer).toList
  
}

case object QASystem {

  def getInstance(config: QAConfig = QAConfig()): Option[QASystem] =
    for (
      paraphraser <- Components.paraphrasers.get(config.paraphraser);
      parser <- Components.parsers.get(config.parser);
      executor <- Components.executors.get(config.executor);
      grouper <- Components.groupers.get(config.grouper);
      scorer <- Components.scorers.get(config.scorer)
    ) yield QASystem(paraphraser, parser, DefaultFilters.wrap(executor), grouper, scorer)

}

case class QAConfig(paraphraser: String = "empty",
  parser: String = "formal",
  executor: String = "identity",
  grouper: String = "basic",
  scorer: String = "numDerivations")

case object Components {
  
  val baseClient = new SolrClient()
  val client = CachedTriplestoreClient(baseClient)
  val paraGenerator = new SolrParaphraseGenerator()
  val paraScorer = new PmiLmScorer()
  val pp = SimpleQuestionParaphraser(paraScorer, paraGenerator)
  val regexParser = RegexQuestionParser()

  val paraphrasers: Map[String, QuestionParaphraser] =
    Map("empty" -> EmptyParaphraser, 
        "templatesLm" -> SimpleQuestionParaphraser(paraScorer, paraGenerator))
  
  val parsers: Map[String, QuestionParser] =
    Map("formal" -> FormalQuestionParser(),
      "keyword" -> StringMatchingParser(client),
      "paralex-old" -> OldParalexParser(),
      "regex" -> regexParser)

  val executors: Map[String, QueryExecutor] =
    Map("identity" -> IdentityExecutor(client),
        "stopword" -> StopwordExecutor(IdentityExecutor(client)),
        "generalize" -> StopwordExecutor(new DiffGeneralizingExecutor(IdentityExecutor(client))),
      "berantRules" -> RelationSynonymExecutor(client, IdentityExecutor(client)),
      "classInstance" -> ClassInstanceExecutor(IdentityExecutor(client)),
      "lexiconArgSyns" -> new LexiconSynonymExecutor(IdentityExecutor(client)),
      "combined" -> new LexiconSynonymExecutor(ClassInstanceExecutor(RelationSynonymExecutor(client, IdentityExecutor(client)))))

  val groupers: Map[String, AnswerGrouper] =
    Map("basic" -> BasicAnswerGrouper(),
      "singleton" -> SingletonAnswerGrouper(),
      "exact" -> ExactAnswerGrouper(),
      "postag sample" -> PostagSampleAnswerGrouper(),
      "postag" -> PostagAnswerGrouper())

  val scorers: Map[String, AnswerScorer] =
    Map("logistic" -> LogisticAnswerScorer(),
      "numDerivations" -> NumDerivationsScorer(),
      "uniform" -> UniformAnswerScorer())
}