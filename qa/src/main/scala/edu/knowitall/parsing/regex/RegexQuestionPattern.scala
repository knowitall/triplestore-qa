package edu.knowitall.parsing.regex

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.Match
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.util.NlpUtils.makeRegex
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.parsing.FormalQuestionParser
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search.{ arg1, rel, arg2 }
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.OpenNlpChunker
import scala.collection.JavaConversions._
import edu.knowitall.execution.ConjunctiveQuery

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

  def parse(tokens: Seq[Lemmatized[ChunkedToken]]): Seq[ListConjunctiveQuery] = {
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
      filledTemplate = filledTemplate.replace("$" + group, text)
    }
    filledTemplate
  }
}

object RegexQuestionPattern {

  private val chunker = new OpenNlpChunker()

  private val formalParser = new FormalQuestionParser()

  def formalParse(filledTemplate: String): List[ListConjunctiveQuery] = formalParser.parse(filledTemplate)

}

object RegexQuestionPatterns {

  val verb =
    // Optional adverb
    "<pos='RB'>? " +
      // Modal or other verbs
      "<pos='VB' | pos='VBP' | pos='VBD' | pos='VBZ' | pos='VBG' | pos='VBN'> " +
      // Optional particle/adverb
      "<pos='RP'>? <pos='RB'>?";

  // old version with modifiers
  //  val word =
  //    "<pos='$' | pos='PRP$' | pos='CD' | pos='DT' | pos='JJ' | pos='JJS' | pos='JJR' | pos='NN' " +
  //      "| pos='NNS' | pos='NNP' | pos='NNPS' | pos='POS' | pos='PRP' | pos='RB' | pos='RBR' | pos='RBS' " +
  //      "| pos='VBN' | pos='VBG'>";

  // split out things OK in middle of a word phrase but not ok to end it
  val modifier = "<pos='$' | pos='PRP$' | pos='DT' | pos='JJ' | pos='JJS' | pos='JJR' " +
    "| pos='POS' | pos='PRP' | pos='RB' | pos='RBR' | pos='RBS' " +
    "| pos='VBN' | pos='VBG' | pos='CC'>";

  val word =
    "< pos='CD' | pos='DT' | pos='NN' " +
      "| pos='NNS' | pos='NNP' | pos='NNPS' | pos='POS' " +
      "| pos='VBN' | pos='VBG'>";

  val prep = "<pos='RB'>? <pos='IN' | pos='TO' | pos='RP'> <pos='RB'>?";

  val rel = s"$verb+ (?:$modifier* $word* $prep)?"

  val property = s"<lemma='be'> <pos='DT'> (${word}+) <lemma='of'>"

  //  val ent = s"$word+"

  val ent = s"(?:$modifier* $word+)+"

  val punct = "<pos='.'>"

  val whatWho = "<lemma='what' | lemma='who'>"

  val whatWhich = "<lemma='what' | lemma='which'>"

  val isa = "\"type\" | \"is a\" | \"is a kind of\" | \"is an example of\""

  val classInst = "<lemma='type' | lemma='kind' | lemma='class' | lemma='sort'>"

  val aux = "<lemma='be' | lemma='have' | lemma='do'>"  
    
  val patterns = Seq(

    // 0. What $R $E? (~9/10)
    new RegexQuestionPattern(
      Seq("rel", "ent"),
      s"$whatWho (<rel>:$rel) (<ent>:$ent) $punct?",
      "($x, $rel, $ent)"),

    // 1. What/who was $e $r $prep? (~9/10)
    new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"$whatWho (<rel1>:$aux) (<ent>:$ent) (<rel2>:$rel $prep+) $punct?",
      "($ent, $rel1 $rel2, $x)"),

    // 2. What/who $r $prep? $e? (~8/10)
    new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"$whatWho (<rel1>:$rel) (<rel2>:$prep?) (<ent>:$ent) $punct?",
      "($x, $rel1 $rel2, $ent)"),

    // 3. What/who $r $obj $prep? $e? (~10/10)
    new RegexQuestionPattern(
      Seq("rel1", "rel2", "rel3", "ent"),
      s"$whatWho (<rel1>:$rel) (<rel2>:$ent) (<rel3>:$prep?) (<ent>:$ent) $punct?",
      "($x, $rel1 $rel2 $rel3, $ent"),

    // 4. When be $e $r? (10/10) (low recall)
    new RegexQuestionPattern(
      Seq("rel1", "rel2", "ent"),
      s"<lemma='when'> (<rel1>:$aux) (<ent>:$ent) (<rel2>:$rel) $punct?",
      Seq("($ent, $rel1 $rel2 in, $x)", "($ent, $rel1 $rel2 on, $x)")),

    // 5. Where did $e $r (10/10) (no unique recall, but probably higher prec)
    new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='where'> $aux (<ent>:$ent) (<rel>:$verb+ $prep+) $punct?",
      "($ent, $rel, $x)"),

    // 6. (10/10, but no unique recall)
    new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='where'> $aux (<ent>:$ent) (<rel>:$rel) $punct?",
      "($ent, $rel in, $x)"),

    // 7. no unique recall
    new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='when'> $aux (<ent>:$ent) (<rel>:$rel) $punct?",
      Seq("($ent, $rel in, $x)", "($ent, $rel on, $x)")),

    // 8. what/who did $e $r (low recall)
    new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"$whatWho $aux (<ent>:$ent) (<rel>:$rel) $punct?",
      "($ent, $rel, $x)"),

    // 9. what $type have/do/be $ent $rel? (low recall)
    new RegexQuestionPattern(
      Seq("ent", "type", "rel"),
      s"<lemma='what' | lemma='which'> (<type>:$ent) $aux (<ent>:$ent) (<rel>:$rel (?:$prep)?) $punct?",
      "$x: ($x, " + isa + ", $type) ($ent, $rel, $x)"),

    // 10. what $type $rel+prep $ent? (What foods are high in protein) (good recall) 9/10 prec.
    new RegexQuestionPattern(
      Seq("type", "rel", "ent"),
      s"$whatWhich (<type>:$ent) (<rel>:$rel) (<ent>:$ent) $punct?",
      "$x: ($x, " + isa + ", $type) ($x, $rel, $ent)"),

    // what is the population of texas => (texas, have population of, ?) (no unique recall) (7/10 prec)
    new RegexQuestionPattern(
      Seq("ent", "rel"),
      s"<lemma='what'> <lemma='be'> <pos='DT'> (<rel>:${word}+) <lemma='of'> (<ent>:$ent) $punct?",
      "$x: ($ent, have $rel of, $x)"),

    // Cars are made in what country -> ($x, type, country) (cars, are made in, $x)
    new RegexQuestionPattern(
      Seq("ent1", "ent2", "rel"),
      s"(<ent1>:$ent) (<rel>:$rel) $whatWhich (<ent2>:$ent) $punct?",
      "$x: ($x, " + isa + ", $ent2) ($ent1, $rel, $x)"),

    // What type of restaurant does Bob like? -> ($x, type, restaurant) (bob, like, $x)
    new RegexQuestionPattern(
      Seq("ent1", "ent2", "rel"),
      s"<lemma='what' | lemma='which'> <lemma='type' | lemma='kind'> <lemma='of'> (<ent1>:$ent) $aux (<ent2>:$ent) (<rel>:$rel) $punct?",
      "$x: ($x, " + isa + ", $ent1) ($ent2, $rel, $x)"),

    // What type of magma is produced by Krakatau?
    new RegexQuestionPattern(
      Seq("ent1", "ent2", "rel"),
      s"<lemma='what' | lemma='which'> $classInst $prep (<ent1>:$ent) (<rel>:$rel) (<ent2>:$ent) $punct?",
      "($x, " + isa + ", $ent1) ($x, $rel, $ent2)"),

    // Hottest place in the US? => ($x, is hottest place in, the us)
    new RegexQuestionPattern(
      Seq("ent1", "ent2", "prep"),
      s"(<ent1>:$ent) (<prep>:$prep) (<ent2>:$ent) $punct?",
      "($x, be $ent1 $prep, $ent2)"))
}
