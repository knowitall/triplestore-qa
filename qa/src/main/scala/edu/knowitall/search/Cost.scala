package edu.knowitall.search

trait Cost[State, Action] extends Function3[State, Action, State, Double]