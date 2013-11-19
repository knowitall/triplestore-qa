package edu.knowitall.apps

import edu.knowitall.scoring.AnswerRanker
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.scoring.ScoredAnswerGroup
import org.slf4j.LoggerFactory
import edu.knowitall.execution.AnswerGrouper
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.execution.ClassInstanceExecutor
import edu.knowitall.execution.StopwordExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.BasicAnswerGrouper
import edu.knowitall.execution.PostagAnswerGrouper
import edu.knowitall.execution.PostagSampleAnswerGrouper
import edu.knowitall.execution.SingletonAnswerGrouper
import edu.knowitall.execution.ExactAnswerGrouper
import edu.knowitall.scoring.NumDerivationsScorer
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.parsing.StringMatchingParser
import edu.knowitall.scoring.UniformAnswerScorer
import edu.knowitall.scoring.AnswerScorer
import edu.knowitall.parsing.OldParalexParser
import edu.knowitall.common.Timing
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.paraphrasing.template.SolrParaphraseGenerator
import com.typesafe.config.ConfigFactory
import edu.knowitall.paraphrasing.Paraphraser
import edu.knowitall.paraphrasing.Paraphrase
import edu.knowitall.paraphrasing.IdentityParaphraser
import edu.knowitall.paraphrasing.template.TemplateParaphraser
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.ExecTuple
import edu.knowitall.paraphrasing.joshua.JoshuaParaphraser
import edu.knowitall.scoring.AnswerDerivationScorer

case class QASystem(paraphraser: Paraphraser,
					parser: QuestionParser,
					executor: QueryExecutor,
					grouper: AnswerGrouper,
					scorer: AnswerScorer) extends AnswerGenerator {

  val conf = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val maxDerivs = conf.getInt("qa.maxDerivs")
  val maxUQueries = conf.getInt("qa.maxUQueries")
  val maxParaphrases = conf.getInt("qa.maxParaphrases")
  val maxAnswerGroups = conf.getInt("qa.maxAnswerGroups")

  def answer(question: String): List[ScoredAnswerGroup] = {

    logger.info(s"Answering question '$question'")

    val derivations = deriveAnswers(question)
    val groups = group(derivations.toList)
    val scored = score(groups)
    scored
  }
  
  def generateAnswers(question: String) = deriveAnswers(question)
  
  def deriveAnswers(question: String): List[AnswerDerivation] = {
    for (pp <- paraphrase(question).par;
         query <- parse(pp);
         execTuple <- execute(query))
      yield AnswerDerivation(question, pp, query, execTuple)
  }.toList

  def paraphrase(question: String): Iterable[Paraphrase] = {
    val allParas = IdentityParaphraser.paraphrase(question) ++
    			paraphraser.paraphrase(question)
    val paras = allParas.distinct.take(maxParaphrases)
    logger.info(s"Paraphrased '$question' to:\n${paras.map(_.target).mkString("\n")}")
    paras
  }

  def parse(pp: Paraphrase): Iterable[ConjunctiveQuery] = {
    val question = pp.target
    val queries = parser.parse(question)
    logger.info(s"Parsed '$question' into queries:\n${queries.mkString("\n")}")
    queries
  }

  def execute(query: ConjunctiveQuery): Iterable[ExecTuple] = {
    val tuples = executor.execute(query)
    logger.info(s"Derived answers:\n${tuples.map(_.answerString).mkString("\n")}")
    tuples
  }

  def group(derivs: Iterable[AnswerDerivation]): Iterable[AnswerGroup] =
    grouper.group(derivs.toList)

  def score(groups: Iterable[AnswerGroup]): List[ScoredAnswerGroup] = {
    val scored = groups.par.map(scorer.scoreAnswer).toList.sortBy(-_.score).take(maxAnswerGroups)
    logger.info(s"Scored answers:\n${scored.map(g => g.score + " " + g.answerString).mkString("\n")}")
    scored
  }

}

case object QASystem {

  def getInstance(config: QAConfig = new QAConfig()): Option[QASystem] =
    for (
      paraphraser <- Components.paraphrasers.get(config.paraphraser);
      parser <- Components.parsers.get(config.parser);
      executor <- Components.executors.get(config.executor);
      grouper <- Components.groupers.get(config.grouper);
      scorer <- Components.scorers.get(config.scorer)
    ) yield QASystem(paraphraser, parser, DefaultFilters.wrap(executor), grouper, scorer)
}

case class QAConfig(paraphraser: String, parser: String, executor: String,
  grouper: String, scorer: String) {
  def this() = this(QAConfig.defaultParaphraser, QAConfig.defaultParser,
      QAConfig.defaultExecutor, QAConfig.defaultGrouper, QAConfig.defaultScorer)
}
case object QAConfig {
  val conf = ConfigFactory.load()
  val defaultParaphraser = conf.getString("qa.defaultParaphraser")
  val defaultParser = conf.getString("qa.defaultParser")
  val defaultExecutor = conf.getString("qa.defaultExecutor")
  val defaultGrouper = conf.getString("qa.defaultGrouper")
  val defaultScorer = conf.getString("qa.defaultScorer")
}

case object Components {

  val baseClient = new SolrClient()
  val client = CachedTriplestoreClient(baseClient)
  val paraGenerator = new SolrParaphraseGenerator()
  val regexParser = RegexQuestionParser()

  val defaults = Map(
      "paraphraser" -> QAConfig.defaultParaphraser,
      "parser" -> QAConfig.defaultParser,
      "executor" -> QAConfig.defaultExecutor,
      "grouper" -> QAConfig.defaultGrouper,
      "scorer" -> QAConfig.defaultScorer)

  val paraphrasers: Map[String, Paraphraser] =
    Map("identity" -> IdentityParaphraser,
        "templatesLm" -> new TemplateParaphraser(),
        "joshua" -> new JoshuaParaphraser())

  val parsers: Map[String, QuestionParser] =
    Map("formal" -> FormalQuestionParser(),
      "keyword" -> StringMatchingParser(client),
      "paralex-old" -> OldParalexParser(),
      "regex" -> regexParser)

  val executors: Map[String, QueryExecutor] =
    Map("identity" -> IdentityExecutor(client),
        "stopword" -> StopwordExecutor(IdentityExecutor(client)),
      "classInstance" -> ClassInstanceExecutor(IdentityExecutor(client)))

  val groupers: Map[String, AnswerGrouper] =
    Map("basic" -> BasicAnswerGrouper(),
      "singleton" -> SingletonAnswerGrouper(),
      "exact" -> ExactAnswerGrouper(),
      "postag sample" -> PostagSampleAnswerGrouper(),
      "postag" -> PostagAnswerGrouper())

  val scorers: Map[String, AnswerScorer] =
    Map("numDerivations" -> NumDerivationsScorer(),
      "uniform" -> UniformAnswerScorer(),
      "derivation" -> new AnswerDerivationScorer())
}