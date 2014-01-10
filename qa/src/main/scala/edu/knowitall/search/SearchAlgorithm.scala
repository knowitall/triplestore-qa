package edu.knowitall.search

import org.slf4j.LoggerFactory

case class Node[State, Action](
    state: State, 
    parent: Option[Edge[State, Action]],
    pathCost: Double) {
  
  def path(rest: List[(State, Action, State)] = Nil): List[(State, Action, State)] = parent match {
    case None => rest
    case Some(e) => e.node.path((e.node.state, e.action, state) :: rest)
  }
  
}

case class Edge[State, Action](action: Action, node: Node[State, Action])

abstract class SearchAlgorithm[State, Action] {
  
  val logger = LoggerFactory.getLogger(this.getClass)

  def problem: SearchProblem[State, Action]
  
  def rootNode: Node[State, Action] = Node(problem.initialState, None, 0.0)
  
  def isGoal(n: Node[State, Action]) = problem.isGoal(n.state) 
  
  def expand(node: Node[State, Action]) = {
    for {
      (action, nextState) <- problem.successors(node.state)
      cost = node.pathCost + problem.cost(node.state, action, nextState)
      edge = Edge(action, node)
    } yield {
      Node(nextState, Some(edge), cost)
    }
  }
  
  def search: List[Node[State, Action]]
}