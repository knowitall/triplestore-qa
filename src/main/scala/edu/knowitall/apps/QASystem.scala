package edu.knowitall.apps

import edu.knowitall.scoring.AnswerScorer
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.scoring.ScoredAnswerGroup
import org.slf4j.LoggerFactory
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGrouper
import edu.knowitall.execution.AnswerGroup
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.parsing.pattern.PatternParser
import edu.knowitall.execution.IdentityExecutor
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
import edu.knowitall.execution.RelationSynonymExecutor

case class QASystem(parser: QuestionParser, executor: QueryExecutor, grouper: AnswerGrouper, scorer: AnswerScorer) {

  val logger = LoggerFactory.getLogger(this.getClass) 
  
  def answer(question: String): List[ScoredAnswerGroup] = {
    
    logger.info(s"Parsing question '$question'")
    val queries = parser.parse(question).take(10)
    answerUQueries(queries, question)
  }
  
  def answerUQueries(uqueries: Iterable[UQuery], question: String): List[ScoredAnswerGroup] = {
    
    logger.info(s"Executing queries for '$question'")
    val derivs = for (query <- uqueries.par; 
                      derivs = executor.deriveAnswers(query);
                      deriv <- derivs) yield deriv
    
    logger.info(s"Grouping answers for '$question'")
    def answerString(group: AnswerGroup) = group.answer.mkString(" ")
    val groups = grouper.group(derivs.toList).sortBy(answerString)
                      
    logger.info(s"Scoring answers for '$question'")
    val answers = for (group <- groups.par;
                       scored = scorer.scoreAnswer(group)) yield scored
    logger.info(s"Returning ${answers.size} answers.")
    answers.toList.sortBy(-_.score)    
  }
}

case class CachedQASystem(qaSystem: QASystem)

case object QASystem {
  
  def getInstance(config: QAConfig = QAConfig()): Option[QASystem] =
    for (parser <- Components.parsers.get(config.parser);
         executor <- Components.executors.get(config.executor);
         grouper <- Components.groupers.get(config.grouper);
         scorer <- Components.scorers.get(config.scorer))
      yield QASystem(parser, executor, grouper, scorer)

}

case class QAConfig(parser: String = "formal", 
                    executor: String = "identity",
                    grouper: String = "basic",
                    scorer: String = "numDerivations")

case object Components {
  
  val baseClient = SolrClient("http://rv-n12.cs.washington.edu:8983/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 100000)
  
  val parsers: Map[String, QuestionParser] =
    Map("formal" -> FormalQuestionParser(),
      "keyword" -> StringMatchingParser(client),
      "paralex-old" -> OldParalexParser(),
      "regex pattern" -> PatternParser())
      
  val executors: Map[String, QueryExecutor] =
    Map("identity" -> IdentityExecutor(client),
        "berantRules" -> RelationSynonymExecutor(client, IdentityExecutor(client)))
  
  val groupers: Map[String, AnswerGrouper] =
    Map("basic" -> BasicAnswerGrouper(),
        "singleton" -> SingletonAnswerGrouper(),
        "exact" -> ExactAnswerGrouper(),
        "postagSample" -> PostagSampleAnswerGrouper(),
        "postag" -> PostagAnswerGrouper())
  
  val scorers: Map[String, AnswerScorer] =
    Map("logistic" -> LogisticAnswerScorer(),
        "numDerivations" -> NumDerivationsScorer(),
      "uniform" -> UniformAnswerScorer()
      )
}