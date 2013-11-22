package edu.knowitall.model

import edu.knowitall.search.qa.QaStep
import edu.knowitall.search.qa.QaState
import edu.knowitall.search.qa.QaAction
import edu.knowitall.learning.HiddenVariableModel
import edu.knowitall.learning.SparseVector
import edu.knowitall.search.qa.QaCostModel
import edu.knowitall.search.qa.QaTransitionModel
import edu.knowitall.search.qa.QaSearchProblem
import edu.knowitall.search.BeamSearch
import com.typesafe.config.ConfigFactory
import edu.knowitall.search.Edge
import edu.knowitall.search.Node
import edu.knowitall.search.qa.AnswerState

case class QaModel(transitionModel: QaTransitionModel = QaModel.defaultTransitionModel, 
				   costModel: QaCostModel = QaModel.defaultCostModel,
				   beamSize: Int = QaModel.defaultBeamSize,
				   goalSize: Int = QaModel.defaultGoalSize) 
				   extends HiddenVariableModel[String, Derivation] {
  
  private def createSearchProblem(question: String) =
    new QaSearchProblem(question, transitionModel, costModel)
  
  private def pathToSteps(q: String, path: List[(QaState, QaAction, QaState)]) =
    path map { case (fromState, action, toState) =>
      QaStep(q, fromState, action, toState)
    }
  
  private def makeDerivation(q: String, n: Node[QaState, QaAction]) = 
    n.state match {
      case as: AnswerState => {
        val a = as.answer
        val steps = pathToSteps(q, n.path())
        val feats = steps.map(costModel.features).fold(SparseVector.zero)(_+_)
        val score = -1 * n.pathCost
        Some(Derivation(q, a, steps.toIndexedSeq, feats, score))
      }
      case _ => None
    }
  
  override def predict(question: String) = {
    val preds = candidatePredictions(question)
    preds.sortBy(-1 * _.score) match {
      case d :: rest => Some(d)
      case _ => None
    }
  }
  
  override def candidatePredictions(question: String) = {
    val problem = createSearchProblem(question)
    val searcher = new BeamSearch(problem, beamSize, goalSize)
    val goals = searcher.search
    goals.flatMap(makeDerivation(question, _))
  }
  
  override def update(q: String, output: Derivation, expected: Derivation) = {
    val delta = expected.features - output.features
    val oldWeights = costModel.weights
    val newWeights = oldWeights + delta
    costModel.weights = newWeights
  }

}

case object QaModel {
  val conf = ConfigFactory.load()
  val defaultBeamSize = conf.getInt("search.beamSize")
  val defaultGoalSize = conf.getInt("search.goalSize")
  lazy val defaultTransitionModel = new QaTransitionModel
  lazy val defaultCostModel = new QaCostModel
}