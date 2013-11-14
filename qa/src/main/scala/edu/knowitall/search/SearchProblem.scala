package edu.knowitall.search

trait SearchProblem[State, Action] {
  def initialState: State
  def successors(s: State): Iterable[(State, Action)]
  def isGoal(s: State): Boolean
  def cost(p: SearchPath[State, Action]): Double
}

case class SearchPath[State, Action](head: State, tail: List[(Action, State)]) {
  def append(s: State, a: Action) = SearchPath(s, (a, head) :: tail)
}