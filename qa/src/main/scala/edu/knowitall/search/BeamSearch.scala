package edu.knowitall.search

import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{Map => MutableMap}
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory

class BeamSearch[State, Action](
    override val problem: SearchProblem[State, Action],
    beam: Beam[State, Action],
    goalSize: Int = BeamSearch.defaultGoalSize,
    maxIters: Int = BeamSearch.defaultMaxIters) extends SearchAlgorithm[State, Action] {
  
  assert(goalSize >= 1)
  
  override val logger = LoggerFactory.getLogger(this.getClass)
  private var iter = 0
  
  private def continueSearch = (goals.size < goalSize) && (!beam.isEmpty) && (iter < maxIters)

  private def searchLoop: Set[Node[State, Action]] = {
    beam.setNodes(rootNode)
    do {
      val initialSize = beam.size 
        
      logger.debug("Expanding frontier")
      val newNodes = beam.nodes.par.flatMap(expand).toList
      logger.debug(s"Expanded to ${newNodes.size} new nodes")
      
      logger.debug("Marking nodes as expanded")
      beam.nodes.foreach(n => expanded.add(n.state))
      
      logger.debug("Adding goal nodes")
      newNodes.filter(isGoal).foreach(addGoalNode)
      
      logger.debug("Updating new frontier")
      beam.setNodes(newNodes.filter(n => !isGoal(n) && !haveExpanded(n)))
      
      val numGoals = newNodes.count(isGoal(_))
      logger.debug(s"Done with search iteration $iter")
      logger.debug(s"Initial frontier size = $initialSize")
      logger.debug(s"Final frontier size = ${beam.size}")
      logger.debug(s"Expanded to ${newNodes.size} new nodes")
      logger.debug(s"Found $numGoals new goal nodes")
      
      iter += 1
      
    } while (continueSearch)
    goals.values.toSet
  }
  
  override def search = searchLoop.toList.sortBy(_.pathCost).take(goalSize)

}

object BeamSearch {
  val conf = ConfigFactory.load()
  val defaultMaxIters = conf.getInt("search.maxIters")
  val defaultGoalSize = conf.getInt("search.goalSize")
}