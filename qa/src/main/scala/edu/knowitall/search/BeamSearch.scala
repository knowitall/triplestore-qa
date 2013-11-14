package edu.knowitall.search


class BeamSearch[State, Action](problem: SearchProblem[State, Action], 
    beamSize: Int) {
  
  type Path = SearchPath[State, Action]
  type Beam = Seq[Path]
  
  def initialBeam = Seq(SearchPath[State, Action](problem.initialState, Nil))
  
  def expandPath(p: Path) = {
    val head = p.head
    val tail = p.tail
    if (problem.isGoal(p.head)) Seq(p)
    else {
      val succ = problem.successors(p.head) map {
        case (s, a) => p.append(s, a)
      }
      succ.toSeq
    }
  }
  
  def nextBeam(beam: Beam) =
    beam.flatMap(expandPath).sortBy(problem.cost).take(beamSize)
    
  def search = searchHelper(initialBeam)
  
  def searchHelper(beam: Beam): Beam = {
    if (beam.forall(path => problem.isGoal(path.head))) beam
    else searchHelper(nextBeam(beam))
  }
  
}