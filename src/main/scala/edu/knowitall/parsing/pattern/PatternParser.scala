package edu.knowitall.parsing.pattern

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.Match
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.taggers.tag.PatternTagger.makeRegex
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search.{arg1, rel, arg2}
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.OpenNlpChunker
import scala.collection.JavaConversions._

sealed trait EntityField
case object Arg1 extends EntityField
case object Arg2 extends EntityField

case class FieldFmt(groups: Seq[String], fmtStrings: Seq[String]) {
  def this(relGroups: String*) = this(relGroups, Seq(Seq.fill(relGroups.size)("%s").mkString(" ")))
  def matchOk(m: Match[Lemmatized[ChunkedToken]]): Boolean = {
    groups.find(g => m.group(g) == null) match {
      case Some(group) => {
        println(group)
        false
      }
      case None => {
        true
      }
    }
  }
  def matchString(m: Match[Lemmatized[ChunkedToken]]): Seq[String] = {
    val strings = groups.map(m.group(_).tokens.map(_.string).mkString(" "))
    fmtStrings.map(_.format(strings:_*))
  }
}

case class QuestionPattern(patString: String, relFmt: FieldFmt, entFmt: FieldFmt, entField: EntityField) {
  import QuestionPattern.lemmatize
  
  val pattern = makeRegex(patString)
  def tryMatch(question: String): Seq[ListConjunctiveQuery] = {
    val lemmas = lemmatize(question)
    val result = pattern.`match`(lemmas)
    if (result != null) {
      
      if (relFmt.matchOk(result) && entFmt.matchOk(result)) getMatches(relFmt.matchString(result), entFmt.matchString(result))
      else Nil
    }
    else 
      Nil
  }
  
  private def getMatches(relMatches: Seq[String], entMatches: Seq[String]) = {
    relMatches.flatMap { relMatch =>
      entMatches.flatMap { entMatch =>
        getMatch(relMatch, entMatch)  
      }  
    }
  }
  
  private def getMatch(relMatch: String, entMatch: String) = {
    val answerVar = TVariable("e")
    val arg1Val = entField match {
      case Arg1 => UnquotedTLiteral(entMatch)
      case Arg2 => answerVar
    }
    val arg2Val = entField match {
      case Arg1 => answerVar
      case Arg2 => UnquotedTLiteral(entMatch)
    }
    val fieldMap: Map[Field, TVal] = Map(arg1 -> arg1Val, rel -> UnquotedTLiteral(relMatch), arg2 -> arg2Val)
    val conjunct = TConjunct("r0", fieldMap)
    Some(ListConjunctiveQuery(List(answerVar), List(conjunct)))
  }
}

object QuestionPattern {
  
  val chunker = new OpenNlpChunker()
  
  def lemmatize(string: String) = chunker.synchronized { chunker(string).toList map MorphaStemmer.stemPostaggedToken }
}

object QuestionPatterns {

  val verb =
    // Optional adverb
    "<pos='RB'>? " +
      // Modal or other verbs
      "<pos='VB' | pos='VBP' | pos='VBD' | pos='VBZ' | pos='VBG' | pos='VBN'> " +
      // Optional particle/adverb
      "<pos='RP'>? <pos='RB'>?";

  val word =
    "<pos='$' | pos='PRP$' | pos='CD' | pos='DT' | pos='JJ' | pos='JJS' | pos='JJR' | pos='NN' " +
      "| pos='NNS' | pos='NNP' | pos='NNPS' | pos='POS' | pos='PRP' | pos='RB' | pos='RBR' | pos='RBS' " +
      "| pos='VBN' | pos='VBG'>";

  val prep = "<pos='RB'>? <pos='IN' | pos='TO' | pos='RP'> <pos='RB'>?";

  val rel = s"$verb+"
  
  val ent = s"$word+"
  
  val punct = "<pos='.'>"

  val whatWho = "<lemma='what' | lemma='who'>"
  
  // What/Who $R $E?
  val p1 = QuestionPattern(s"$whatWho (<rel>:$rel) (<ent>:$ent) $punct?", new FieldFmt("rel"), new FieldFmt("ent"), Arg2)
  
   
  
  // What/who was $e $r $prep?
  val p2 = QuestionPattern(
      s"$whatWho (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel $prep+) $punct?", new FieldFmt("rel1", "rel2"), new FieldFmt("ent"), Arg1)
  
  //What/who $r $prep? $e?   
   
  val p3 = QuestionPattern(s"$whatWho (<rel1>:$rel) (<rel2>:$prep?) (<ent>:$ent) $punct?", new FieldFmt("rel1", "rel2"), new FieldFmt("ent"), Arg2)
  
  //What/who $r $obj $prep? $e?   
  val p4 = QuestionPattern(s"$whatWho (<rel1>:$rel) (<rel2>:$ent) (<rel3>:$prep?) (<ent>:$ent) $punct?", new FieldFmt("rel1", "rel2", "rel3"), new FieldFmt("ent"), Arg2)
      
  // When be $e $r?
  val p5 = QuestionPattern(s"<lemma='when'> (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel) $punct?", new FieldFmt(Seq("rel1", "rel2"), Seq("%s %s in", "%s %s on")), new FieldFmt("ent"), Arg1)
  // Where be $e $r?
  val p6 = QuestionPattern(s"<lemma='where'> (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel) $punct?", new FieldFmt(Seq("rel1", "rel2"), Seq("%s %s in")), new FieldFmt("ent"), Arg1)
  
  // Where did $e $r
  val p7 = QuestionPattern(s"<lemma='where'> <lemma='do'> (<ent>:$ent) (<rel>:$rel $prep+) $punct?", new FieldFmt("rel"), new FieldFmt("ent"), Arg1)
  val p8 = QuestionPattern(s"<lemma='where'> <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", new FieldFmt(Seq("rel"), Seq("%s in")), new FieldFmt("ent"), Arg1)

  // When did $e $r
  val p9 = QuestionPattern(s"<lemma='when'> <lemma='do'> (<ent>:$ent) (<rel>:$rel $prep+) $punct?", new FieldFmt("rel"), new FieldFmt("ent"), Arg1)
  val p10 = QuestionPattern(s"<lemma='when'> <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", new FieldFmt(Seq("rel"), Seq("%s in", "%s on")), new FieldFmt("ent"), Arg1)
  
  // what/who did $e $r
  val p11 = QuestionPattern(s"$whatWho <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", new FieldFmt("rel"), new FieldFmt("ent"), Arg1)
    
  val patterns = Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
}

case class PatternParser() extends QuestionParser {
  
  import QuestionPatterns.patterns
  
  def parse(question: String) = patterns.flatMap(p => p.tryMatch(question)) 
}

object PatternParser extends App { 
  
  import QuestionPatterns.patterns
  
  io.Source.stdin.getLines foreach { line =>
    val matchingPatterns = patterns.zipWithIndex.map { case (p, i) => (i+1, p.tryMatch(line))}
    matchingPatterns.foreach { case (i, queries) =>
      if (queries.isEmpty) println(s"$i\tNil")
      else queries.foreach { q =>
        println(s"$i\t${q.conjuncts.head}")  
      }      
    }
  }   
}