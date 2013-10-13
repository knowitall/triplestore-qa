package edu.knowitall.execution

import edu.knowitall.apps.AnswerDerivation

trait AnswerGrouper {
  def group(derivs: List[AnswerDerivation]): List[AnswerGroup]
  
  def regroup(groups: List[AnswerGroup]): List[AnswerGroup] = {
    val derivations = groups.flatMap(g => g.derivations)
    group(derivations)
  }
}

trait AnswerGroup {
  def answer: List[String]
  def answerString: String = answer.mkString(" ")
  def alternates: List[List[String]]
  def derivations: List[AnswerDerivation]
}

case class BasicAnswerGroup(answer: List[String], alternates: List[List[String]], 
    derivations: List[AnswerDerivation]) extends AnswerGroup 
    
case class BasicAnswerGrouper(
    norm: String => String = BasicAnswerGrouper.normalize)
    extends AnswerGrouper {
  
  override def group(derivations: List[AnswerDerivation]): List[BasicAnswerGroup] = {
    val grouped = derivations.groupBy(d => d.answer.map(norm))
    val groups = for ((answers, grp) <- grouped;
                      alts = grp.map(_.answer).distinct) 
                      yield BasicAnswerGroup(answers, alts, grp)
    groups.toList
  }
  
}

case class PostagSampleAnswerGrouper() extends AnswerGrouper {
  
  val postagGrouper = PostagAnswerGrouper()

  private def sampleDerivations(group: BasicAnswerGroup) = {
    val n = math.sqrt(group.derivations.size).ceil.toInt
    val execTuples = group.derivations
    val grouped = execTuples.groupBy(_.answer).iterator.toSeq
    val sorted = grouped.sortBy(-_._2.size).map(_._2)
    val groupTruncated = sorted.flatMap { derivs =>
      val m = math.sqrt(derivs.size).ceil.toInt
      derivs.take(m)
    }
    val sampled = groupTruncated.take(n).toList
    group.copy(derivations = sampled)
  }
 
  override def group(derivations: List[AnswerDerivation]): List[BasicAnswerGroup] = {
    val postagGroups = postagGrouper.group(derivations)
    val sampledGroups = postagGroups map sampleDerivations
    sampledGroups.toList
  }
} 

case class PostagAnswerGrouper() extends AnswerGrouper {

  val basicGrouper = BasicAnswerGrouper(PostagAnswerGrouper.normalize)
  
  private def renormAlts(normAnswer: String => String)(group: BasicAnswerGroup) = {
    group
      .copy(alternates = group.alternates map { alt => alt map normAnswer })
  }

  override def group(derivations: List[AnswerDerivation]): List[BasicAnswerGroup] = {
    val basicGroups = basicGrouper.group(derivations)
    val postagGroups = basicGroups map renormAlts(PostagAnswerGrouper.normalize)
    val postags = postagGroups.map(g => (g.answer, g.derivations.size))
    postagGroups
  }
}

case object PostagAnswerGrouper { 
  
  val cleanRegex = "NNP(\\sNNP)+".r.pattern 
  
  // merge together more than two NNPS
  def clean(string: String) = cleanRegex.matcher(string).replaceAll("NNP NNP")
  
  def normalize(s: String): String = {
    val basic = StrSim.normTokens(s).mkString(" ")
    val lemmas = StrSim.lemmatize(s)
    val filtered = lemmas.filterNot(_.isPunctuation)
    val postags = filtered.map { token => token.postag }
    
    if (postags.nonEmpty) 
      clean(postags.mkString(" "))
    else
     "Nil"
  }
}



case object BasicAnswerGrouper {
  def normalize(s: String): String = StrSim.norm(s)
}

case class SingletonAnswerGrouper() extends AnswerGrouper {
  def group(derivations: List[AnswerDerivation]): List[AnswerGroup] = {
    val groups = for (execTuple <- derivations;
                      ans = execTuple.answer;
                      alts = List(execTuple.answer);
                      grp = List(execTuple)) 
                      yield BasicAnswerGroup(ans, alts, grp)
    groups.toList
  }
}

case class ExactAnswerGrouper() extends AnswerGrouper {
  val identityGrouper = BasicAnswerGrouper(identity)
  def group(derivations: List[AnswerDerivation]) = identityGrouper.group(derivations)
}

