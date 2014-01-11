package edu.knowitall.search

import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{Map => MutableMap}
import com.google.common.collect.MinMaxPriorityQueue
import scala.collection.JavaConversions._

class BestFirstSearch[State, Action](override val problem: SearchProblem[State, Action], beamSize: Int, goalSize: Int) extends SearchAlgorithm[State, Action] {
  
  private val beam = MinMaxPriorityQueue.maximumSize(beamSize).create[Node[State, Action]]()
  
  override def continueSearch = (beam.size > 0) && (goals.size < goalSize)
  
  override def initialize = beam.add(rootNode)
  
  override def searchIter = {

    val node = beam.pollFirst
    val successors = expand(node).toList
      
    val (newGoals, newNodes) = successors.partition(isGoal)
    newGoals.foreach(addGoalNode)
    beam.addAll(newNodes)

  }

}