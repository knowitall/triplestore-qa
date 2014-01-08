package edu.knowitall.relsyn

import edu.knowitall.execution.TConjunct

trait RelSynClient {
  def relSyns(s: String, limit: Int): List[RelSynRule]
  def relSyns(c: TConjunct): List[RelSynRule]
}