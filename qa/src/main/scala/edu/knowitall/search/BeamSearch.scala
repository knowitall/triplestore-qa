package edu.knowitall.search

import scala.collection.mutable.{SortedSet => MutableSortedSet}
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{Map => MutableMap}
import org.slf4j.LoggerFactory
import scala.math.Ordering.Implicits._

class BeamSearch[State, Action](
    override val problem: SearchProblem[State, Action],
    beamSize: Int,
    goalSize: Int) extends SearchAlgorithm[State, Action] {
  
  assert(goalSize >= 1)
  assert(beamSize >= 1)
  
  override val logger = LoggerFactory.getLogger(this.getClass)
  private val costOrdering = Ordering.by {n: Node[State, Action] => (n.pathCost, n.state.hashCode)}
  private var frontier = List(rootNode)//MutableSortedSet()(costOrdering)
  private val expanded = MutableSet.empty[State]
  private val goals = MutableSet.empty[Node[State, Action]]
  //frontier += rootNode
      
  private def setFrontier(nodes: Iterable[Node[State, Action]]) = {
    val distinctNodes = nodes.groupBy(_.state) map {
      case (state, group) => group.minBy(_.pathCost)
    }
    frontier = distinctNodes.toList.sortBy(_.pathCost).take(beamSize)
  }
  
  private def addGoalNode(node: Node[State, Action]) = {
    assert(isGoal(node))
    goals.add(node)
  }
  
  private def haveExpanded(n: Node[State, Action]) = expanded.contains(n.state)
  
  private def continueSearch = (goals.size < goalSize) && (frontier.size > 0)
  
  private def searchLoop: Set[Node[State, Action]] = {
    var iter = 1
    do {
      val initialSize = frontier.size 
        
      logger.debug("Expanding frontier")
      val newNodes = frontier.par.flatMap(expand).toList
      logger.debug(s"Expanded to ${newNodes.size} new nodes")
      
      logger.debug("Marking nodes as expanded")
      frontier.foreach(n => expanded.add(n.state))
      
      logger.debug("Adding goal nodes")
      newNodes.filter(isGoal).foreach(addGoalNode)
      
      logger.debug("Updating new frontier")
      setFrontier(newNodes.filter(n => !isGoal(n) && !haveExpanded(n)))
      
      val numGoals = newNodes.count(isGoal(_))
      logger.debug(s"Done with search iteration $iter")
      logger.debug(s"Initial frontier size = $initialSize")
      logger.debug(s"Final frontier size = ${frontier.size}")
      logger.debug(s"Expanded to ${newNodes.size} new nodes")
      logger.debug(s"Found $numGoals new goal nodes")
      
      iter += 1
      
    } while (continueSearch)
    goals.toSet
  }
  
  def search = searchLoop.toList.sortBy(_.pathCost).take(goalSize)

}