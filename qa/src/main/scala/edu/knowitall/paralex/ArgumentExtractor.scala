package edu.knowitall.paralex

import edu.knowitall.parsing.QWord
import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.parsing.ArgVar
import edu.knowitall.tool.tokenize.ClearTokenizer

class ArgumentExtractor(maxSize: Int = 4) {
  
  val allowedTags = Set("$", "PRP$", "CD", "DT", "JJ", "JJS", "JJR", "NN", 
      "NNS", "NNP", "NNPS", "POS", "PRP", "RB", "RBR", "RBS", "VBN", "VBG", "IN")

  val stopTags = Set("IN", "TO", "DT", "PRP$")
      
  val tagger = new StanfordPostagger()
  val tokenizer = new ClearTokenizer()
  
  def isStopTag(sub: Seq[PostaggedToken]): Boolean = 
    sub.map(t => t.postag).toSet.subsetOf(stopTags)
    
  def startsWithPrep(sub: Seq[PostaggedToken]): Boolean = sub match {
    case x :: rest => x.postag == "IN"
    case _ => false
  }
  
  def detInPattern(i: Int, ws: Seq[PostaggedToken]): Boolean =
     (i > 0) && (ws.size > 0) && (ws(i-1).postag == "DT")

  
  def keepArg(i: Int, j: Int, ws: Seq[PostaggedToken], sub: Seq[PostaggedToken]): Boolean = 
    !isStopTag(sub) && sub.forall(t => allowedTags.contains(t.postag)) && !startsWithPrep(sub) && !detInPattern(i, ws)
    
  def intervals(size: Int, max: Int) =
    for (i <- Range(0, size); j <- Range(i, size); if j+1-i <= max) yield (i, j+1)
    
  def templatize(ws: Seq[PostaggedToken]): Seq[AbstractedArg] = {
      val n = ws.size
      for ((i, j) <- intervals(n, maxSize);
    	   arg = ws.slice(i, j);
    	   if keepArg(i, j, ws, arg);
    	   left = ws.slice(0, i).map(t => QWord(t.string));
    	   right = ws.slice(j, n).map(t => QWord(t.string));
    	   newArg = arg.map(t => QWord(t.string));
    	   abs = AbstractedArg(newArg, left ++ List(ArgVar) ++ right)
    	   ) yield abs
  }
  
  def arguments(q: Seq[QWord]): Seq[AbstractedArg] = {
    val ts = q.map(w => new Token(w.word, 0))
    val tagged = tagger.postagTokens(ts)
    templatize(tagged)
  }
  
  def arguments(s: String): Seq[AbstractedArg] = arguments(tokenizer(s).map(t => QWord(t.string)))
}

object ArgumentExtractor {
  
}