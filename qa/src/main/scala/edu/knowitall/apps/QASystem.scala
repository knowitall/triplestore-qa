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

case class QASystem(parser: QuestionParser, executor: QueryExecutor, grouper: AnswerGrouper, scorer: AnswerScorer) {

  val conf = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val maxDerivs = conf.getInt("qa.maxDerivs")
  val maxUQueries = conf.getInt("qa.maxUQueries")

  def answer(question: String): List[ScoredAnswerGroup] = {

    logger.info(s"Parsing question '$question'")
    val queries = parser.parse(question).take(maxUQueries)
    answerUQueries(queries, question)
  }

  def answerUQueries(uqueries: Iterable[UQuery], question: String): List[ScoredAnswerGroup] = {

    logger.info(s"Executing queries for '$question'")
    val (nsExec, derivs) = Timing.time {
      for (
        query <- uqueries.par;
        derivs = executor.deriveAnswers(query);
        deriv <- derivs
      ) yield deriv
    }

    logger.info("Executed queries in " + Timing.Seconds.format(nsExec))
    
    logger.info(s"Grouping answers for '$question'")
    def answerString(group: AnswerGroup) = group.answer.mkString(" ")
    val groups = grouper.group(derivs.take(maxDerivs).toList).sortBy(answerString)

    logger.info(s"Scoring answers for '$question'")
    val answers = for (
      group <- groups.par;
      scored = scorer.scoreAnswer(group)
    ) yield scored
    logger.info(s"Returning ${answers.size} answers.")
    answers.toList.sortBy(-_.score)
  }
}

case object QASystem {

  def getInstance(config: QAConfig = QAConfig()): Option[QASystem] =
    for (
      parser <- Components.parsers.get(config.parser);
      executor <- Components.executors.get(config.executor);
      grouper <- Components.groupers.get(config.grouper);
      scorer <- Components.scorers.get(config.scorer)
    ) yield QASystem(parser, DefaultFilters.wrap(executor), grouper, scorer)

}

case class QAConfig(parser: String = "formal",
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

  val parsers: Map[String, QuestionParser] =
    Map("formal" -> FormalQuestionParser(),
      "keyword" -> StringMatchingParser(client),
      "paralex-old" -> OldParalexParser(),
      "regex" -> regexParser,
      "paralex" -> new ParalexQuestionParser(pp, regexParser))

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