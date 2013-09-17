package edu.knowitall.parsing.regex

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
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search.{arg1, rel, arg2}
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.OpenNlpChunker
import scala.collection.JavaConversions._


/**
 * A Regex-based question pattern. Capture groups in pattern
 * are then substituted into template. Within template,
 * group names should be prefixed with a dollar sign. Template
 * should fill to become a valid formal query.
 * Example:
 * 
 * groups: Seq("r1", "e1")
 * pattern: "<lemma='what'> (<r1>:<pos='VB'>+) (<e1>:<pos='NN'>+)"
 * template: ($x, $r1, $e1)
 * 
 * Unused variables (e.g. $x) will be left in their literal form.
 */
case class RegexQuestionPattern(val groups: Seq[String], pattern: String, templates: Seq[String]) {

  def this(groups: Seq[String], pattern: String, template: String) = this(groups, pattern, Seq(template))
  
  val regex = makeRegex(pattern)
  
  def parse(tokens: Seq[Lemmatized[ChunkedToken]]): Seq[UQuery] = {
    val tryMatch = regex.`match`(tokens)
    if (tryMatch == null || tryMatch.isEmpty()) return Seq.empty
    else {
      val groupTexts = groups.map({ g => 
        val groupLookup = tryMatch.group(g)
        require(groupLookup != null, s"Group $g not found in pattern.")
        (g, groupLookup.tokens.map(_.string).mkString(" "))
      })
      val filledTemplates = templates.map(t => fillTemplate(groupTexts, t))
      filledTemplates flatMap RegexQuestionPattern.formalParse      
    }
  }
  
  def fillTemplate(groupTexts: Seq[(String, String)], template: String): String = {
    var filledTemplate = template
    for ((group, text) <- groupTexts) {
      filledTemplate = filledTemplate.replace("$"+group, text)
    }
    filledTemplate
  }
}

object RegexQuestionPattern {
  
  private val chunker = new OpenNlpChunker()
  
  private val formalParser = new FormalQuestionParser()
  
  def formalParse(filledTemplate: String) = formalParser.parse(filledTemplate)
  
}

object RegexQuestionPatterns {

  private val verb =
    // Optional adverb
    "<pos='RB'>? " +
      // Modal or other verbs
      "<pos='VB' | pos='VBP' | pos='VBD' | pos='VBZ' | pos='VBG' | pos='VBN'> " +
      // Optional particle/adverb
      "<pos='RP'>? <pos='RB'>?";

  private val word =
    "<pos='$' | pos='PRP$' | pos='CD' | pos='DT' | pos='JJ' | pos='JJS' | pos='JJR' | pos='NN' " +
      "| pos='NNS' | pos='NNP' | pos='NNPS' | pos='POS' | pos='PRP' | pos='RB' | pos='RBR' | pos='RBS' " +
      "| pos='VBN' | pos='VBG'>";

  private val prep = "<pos='RB'>? <pos='IN' | pos='TO' | pos='RP'> <pos='RB'>?";

  private val rel = s"$verb+ (?:$word* $prep)?"
  
  private val ent = s"$word+"
  
  private val punct = "<pos='.'>"

  private val whatWho = "<lemma='what' | lemma='who'>"
  
  private val whatWhich = "<lemma='what' | lemma='which'>"
    
  private val isa = "\"type\" | \"is a\" | \"is a kind of\" | \"is an example of\"" 

  val patterns = Seq( 

  // 0. What/Who $R $E?
  new RegexQuestionPattern(
      Seq("rel", "ent"), 
      s"$whatWho (<rel>:$rel) (<ent>:$ent) $punct?", 
      "($x, $rel, $ent)"),
  
  // 1. What/who was $e $r $prep?
  new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"), 
      s"$whatWho (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel $prep+) $punct?", 
      "($ent, $rel1 $rel2, $x)"),
  
  // 2. What/who $r $prep? $e?   
  new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"$whatWho (<rel1>:$rel) (<rel2>:$prep?) (<ent>:$ent) $punct?", 
      "($x, $rel1 $rel2, $ent)"),
  
  // 3. What/who $r $obj $prep? $e?   
  new RegexQuestionPattern(
      Seq("rel1", "rel2", "rel3", "ent"),
      s"$whatWho (<rel1>:$rel) (<rel2>:$ent) (<rel3>:$prep?) (<ent>:$ent) $punct?", 
      "($x, $rel1 $rel2 $rel3, $ent"),
      
  // When be $e $r?
  new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"<lemma='when'> (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel) $punct?", 
      Seq("($ent, $rel1 $rel2 in, $x)", "($ent, $rel1 $rel2 on, $x)")),
      
  // Where be $e $r?
  new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"<lemma='where'> (<rel1>:<lemma='be'>) (<ent>:$ent) (<rel2>:$rel) $punct?", 
      "($ent, $rel1 $rel2 in, $x)"),
  
  // Where did $e $r
  new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='where'> <lemma='do'> (<ent>:$ent) (<rel>:$verb+ $prep+) $punct?", 
      "($ent, $rel, $x)"),
  
  new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='where'> <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", 
      "($ent, $rel in, $x)"),

  // When did $e $r
  new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='when'> <lemma='do'> (<ent>:$ent) (<rel>:$rel $prep+) $punct?", 
      "($ent, $rel, $x)"),
  
  new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='when'> <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", 
      Seq("($ent, $rel in, $x)", "($ent, $rel on, $x)")),
  
  // what/who did $e $r
  new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"$whatWho <lemma='do'> (<ent>:$ent) (<rel>:$rel) $punct?", 
      "($ent, $rel, $x)"),
  
  // what $type have/do/be $ent $rel?
  new RegexQuestionPattern(
      Seq("ent", "type", "rel"),
      s"<lemma='what' | lemma='which'> (<type>:$ent) <lemma='do' | lemma='have' | lemma='be'> (<ent>:$ent) (<rel>:$rel (?:$prep)?) $punct?",
      "$x: ($x, "+isa+", $type) ($ent, $rel, $x)"),
      
  // what $type $rel+prep $ent? (What foods are high in protein)
  new RegexQuestionPattern(
    Seq("type", "rel", "ent"),
    s"$whatWhich (<type>:$ent) (<rel>:$rel (?:$prep)?) (<ent>:$ent) $punct?",
    "$x: ($x, "+isa+", $type) ($x, $rel, $ent)")
  )
}
