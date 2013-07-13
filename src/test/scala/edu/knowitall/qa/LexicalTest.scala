package edu.knowitall.qa
import org.scalatest.FlatSpec
import edu.knowitall.collection.immutable.Interval

class LexicalTest extends FlatSpec {
  
  def iv(i: Integer, j: Integer) = Interval.open(i, j)
  
  // An entity item 
  val joe = EntItem(IndexedSeq("joe"), "joe_ent")
  
  // Two relation items which have inverse semantics
  val r1 = RelItem(IndexedSeq("like"), "like_rel", Arg2First)
  val r2 = RelItem(IndexedSeq("like"), "liked-by_rel", Arg1First)
  
  // The question template "who $r $e"
  val qt1 = QuestionItem(IndexedSeq(QWord("who"), RelVar, ArgVar), Arg2First)
  
  // The question template "who does $e $r", which has inverse semantics as
  // the template q1
  val qt2 = QuestionItem(IndexedSeq(QWord("who"), QWord("do"), ArgVar, RelVar), 
      Arg1First)
  
  // One-argument question templates
  val rqt1 = QuestionRelItem(IndexedSeq(QWord("who"), QWord("like"), ArgVar), 
      "like_rel", Arg2First)
  val rqt2 = QuestionRelItem(IndexedSeq(QWord("who"), QWord("like"), ArgVar), 
      "liked-by_rel", Arg1First)
  
  // Make a lexicon from the items
  val lexicon = MapLexicon(List(joe, r1, r2, qt1, qt2, rqt1, rqt2))
  val parser = BottomUpParser(lexicon)
  
  // A few questions
  val q1: IndexedSeq[QWord] = IndexedSeq("who", "do", "joe", "like")
  val q2: IndexedSeq[QWord] = IndexedSeq("who", "like", "joe")
  
  "BottomUpParser" should "parse a few simple questions" in {
    
    val d1 = TwoArgDerivation(q1, qt2, Span(iv(3, 4), r1), Span(iv(2, 3), joe))
    val d2 = TwoArgDerivation(q1, qt2, Span(iv(3, 4), r2), Span(iv(2, 3), joe))
    
    val derivs1 = parser.parse(q1).toSet
    assert(derivs1 === Set(d1, d2))

    val d3 = TwoArgDerivation(q2, qt1, Span(iv(1, 2), r1), Span(iv(2, 3), joe))
    val d4 = TwoArgDerivation(q2, qt1, Span(iv(1, 2), r2), Span(iv(2, 3), joe))
    val d5 = OneArgDerivation(q2, rqt1, Span(iv(2, 3), joe))
    val d6 = OneArgDerivation(q2, rqt2, Span(iv(2, 3), joe))
    
    val derivs2 = parser.parse(q2).toSet
    
    assert(derivs2 === Set(d3, d4, d5, d6))
    
  }


}