package edu.knowitall.apps

import edu.knowitall.scoring.AnswerScorer
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.scoring.ScoredAnswer
import org.slf4j.LoggerFactory
import edu.knowitall.execution.AnswerDerivation

case class QASystem(parser: QuestionParser, executor: QueryExecutor, scorer: AnswerScorer) {

  val logger = LoggerFactory.getLogger(this.getClass) 
  
  def answer(question: String): List[ScoredAnswer] = {
    
    logger.info(s"Parsing question '$question'")
    val queries = parser.parse(question)
    
    logger.info(s"Executing queries for '$question'")
    val derivs = for (query <- queries; 
                      derivs = executor.deriveAnswers(query);
                      deriv <- derivs) yield deriv
    
    logger.info(s"Scoring answers for '$question'")
    val groupedByAnswer = derivs.toList.groupBy(_.answer)
    val answers = for ((answer, ds) <- groupedByAnswer;
                       scored = scorer.scoreAnswer(answer, ds)) yield scored
    answers.toList
    
  }
  
}