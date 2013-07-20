package edu.knowitall.apps

import edu.knowitall.scoring.AnswerScorer
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.scoring.ScoredAnswerGroup
import org.slf4j.LoggerFactory
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.AnswerGrouper
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.BasicAnswerGrouper
import edu.knowitall.scoring.NumDerivationsScorer
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.parsing.StringMatchingParser
import edu.knowitall.scoring.UniformAnswerScorer
import edu.knowitall.parsing.OldParalexParser
import edu.knowitall.execution.RelationSynonymExecutor

case class QASystem(parser: QuestionParser, executor: QueryExecutor, grouper: AnswerGrouper, scorer: AnswerScorer) {

  val logger = LoggerFactory.getLogger(this.getClass) 
  
  def answer(question: String): List[ScoredAnswerGroup] = {
    
    logger.info(s"Parsing question '$question'")
    val queries = parser.parse(question).take(10)
    
    logger.info(s"Executing queries for '$question'")
    val derivs = for (query <- queries.par; 
                      derivs = executor.deriveAnswers(query);
                      deriv <- derivs) yield deriv
    
    logger.info(s"Grouping answers for '$question'")
    val groups = grouper.group(derivs.toList)
                      
    logger.info(s"Scoring answers for '$question'")
    val answers = for (group <- groups.par;
                       scored = scorer.scoreAnswer(group)) yield scored
    answers.toList.sortBy(-_.score)
    
  }
  
}
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
  
  val baseClient = SolrClient("http://rv-n12:8983/solr/triplestore", 500)
  val client = CachedTriplestoreClient(baseClient, 100000)
  
  val parsers: Map[String, QuestionParser] =
    Map("formal" -> FormalQuestionParser(),
      "keyword" -> StringMatchingParser(client),
      "paralex-old" -> OldParalexParser())
      
  val executors: Map[String, QueryExecutor] =
    Map("identity" -> IdentityExecutor(client),
        "berantRules" -> RelationSynonymExecutor(client, IdentityExecutor(client)))
  
  val groupers: Map[String, AnswerGrouper] =
    Map("basic" -> BasicAnswerGrouper())
  
  val scorers: Map[String, AnswerScorer] =
    Map("numDerivations" -> NumDerivationsScorer(),
      "uniform" -> UniformAnswerScorer())
}