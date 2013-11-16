package edu.knowitall.search

import scala.collection.mutable.{SortedSet => MutableSortedSet}
import scala.collection.mutable.{Set => MutableSet}
import org.slf4j.LoggerFactory
import scala.math.Ordering.Implicits._

class BeamSearch[State, Action](
    override val problem: SearchProblem[State, Action],
    beamSize: Int,
    goalSize: Int,
    numParallel: Int = 1) extends SearchAlgorithm[State, Action] {
  
  assert(goalSize >= 1)
  assert(beamSize >= 1)
  assert(numParallel >= 1)
  
  val logger = LoggerFactory.getLogger(this.getClass)
  private val costOrdering = Ordering.by {n: Node[State, Action] => (n.pathCost, n.state.hashCode)}
  private val frontier = MutableSortedSet()(costOrdering)
  private val visited = MutableSet.empty[State]
  private val goals = MutableSet.empty[Node[State, Action]]
  frontier += rootNode
  
  private def takeFromFrontier(n: Int) = {
    val taken = frontier.take(n)
    frontier --= taken
    taken
  }
  
  private def resizeFrontier =
    while (frontier.size > beamSize) frontier -= frontier.last
      
  private def addToFrontier(nodes: Iterable[Node[State, Action]]) = {
    nodes foreach frontier.add
    if (frontier.size > beamSize) resizeFrontier
  }
  
  private def addGoalNode(node: Node[State, Action]) = {
    assert(isGoal(node))
    goals.add(node)
    logger.debug(s"Added new goal state: ${node.state}")
  }
  
  private def continueSearch = (goals.size < goalSize) && (frontier.size > 0)
  
  private def searchLoop: Set[Node[State, Action]] = {
    do {
      val toExpand = takeFromFrontier(numParallel)
      val expanded = toExpand.par.flatMap(expand).toList
      toExpand.foreach(n => visited.add(n.state))
      expanded.filter(isGoal).foreach(addGoalNode)
      addToFrontier(expanded.filter(n => !isGoal(n) && !visited.contains(n.state)))
    } while (continueSearch)
    goals.toSet
  }
  
  def search = searchLoop.toList.sortBy(_.pathCost).take(goalSize)

}