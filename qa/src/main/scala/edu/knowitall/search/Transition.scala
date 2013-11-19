package edu.knowitall.search

trait Transition[State, Action] extends Function[State, Iterable[(Action, State)]]