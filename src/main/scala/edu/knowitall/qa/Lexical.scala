package edu.knowitall.qa

import edu.knowitall.collection.immutable.Interval
import scala.language.implicitConversions
//import PartialFunction._

/* Question Tokens are either strings, or two special tokens used to represent
 * variables (RelVar or $r, and ArgVar or $e).
 */
trait QToken
object QToken {
  def qTokenWrap(s: String): QToken = s match {
    case "$r" => RelVar
    case "$y" => ArgVar
    case _ => QWord.qWordWrap(s)
  }
}
case class QWord(word: String) extends QToken {
  override def toString = word
}
object QWord {
  implicit def qWordWrap(s: String) = new QWord(s)
}
case object RelVar extends QToken {
  override def toString = "$r"
}
case object ArgVar extends QToken {
  override def toString = "$y"
}

/* Argument orderings are used to represent whether an argument binds to the
 * arg1 slot first, or the arg2 slot. Composing two functions will possibly
 * result in a different argument ordering.
 */
sealed abstract trait ArgOrder
case object Arg1First extends ArgOrder
case object Arg2First extends ArgOrder
object ArgOrder {
  def compose(o1: ArgOrder, o2: ArgOrder) = (o1, o2) match {
    case (Arg1First, Arg1First) => Arg1
    case (Arg1First, Arg2First) => Arg2
    case (Arg2First, Arg1First) => Arg2
    case (Arg2First, Arg2First) => Arg1
  }
  def toInt(o: ArgOrder) = o match {
    case Arg1First => 1
    case Arg2First => 0
  }
  def queryArg(o: ArgOrder) = o match {
    case Arg1First => Arg2
    case Arg2First => Arg1
  }
  def fromInt(i: Int): ArgOrder = i match {
    case 0 => Arg2First
    case 1 => Arg1First
    case _ => throw new IllegalArgumentException("Invalid argument ordering encoding.")
  }
}

/* Constant symbols used to represent Arg1 and Arg2 slots. */
sealed abstract trait Arg
case object Arg1 extends Arg
case object Arg2 extends Arg

/* An abstract "simple" query against a triplestore. Has the form either
 * (?, relation, entity) or (entity, relation, ?) and represents querying a
 * triplestore for all arguments that can be plugged in for ?.
 */
case class SimpleQuery(relation: String, entity: String, queryField: Arg) {
  override def toString = queryField match {
    case Arg1 => s"(?, $relation, $entity)"
    case Arg2 => s"($entity, $relation, ?)"
  }
}

/* Lexical Items associate question tokens with some semantic interpretation. */
sealed trait LexItem {
  val words: IndexedSeq[QToken]
}

/* An entity lexical item associates question words (not including question 
 * variables $r or $e) with a string entity.
 */
case class EntItem(val words: IndexedSeq[QWord], val entity: String) extends LexItem {
  val ws = words.mkString(" ")
  override def toString = s"$ws = $entity"
}

/* A relation lexical item associates question words (not including question 
 * variables $r or $e) with a string relation and an argument ordering.
 */
case class RelItem(val words: IndexedSeq[QWord], val relation: String, val argOrder: ArgOrder) extends LexItem {
  val ws = words.mkString(" ")
  override def toString = s"$ws = " + { argOrder match {
    case Arg2First => s"λyλx. (x, $relation, y)"
    case Arg1First => s"λyλx. (y, $relation, x)"
  } }
}

/* A question lexical item associates question words that have exactly one
 * relation variable $r and one entity variable $e with an argument ordering.
 */    
case class QuestionItem(val words: IndexedSeq[QToken], val argOrder: ArgOrder) extends LexItem {
   
  if (words.count(_ == RelVar) != 1) throw new 
  	IllegalArgumentException("QuestionItem must contain one RelVar: " + words)
  if (words.count(_ == ArgVar) != 1) throw new 
  	IllegalArgumentException("QuestionItem must contain one ArgVar: " + words)

  val ws = words.mkString(" ")
  override def toString = s"$ws = " + { argOrder match {
    case Arg2First => "λrλyλx r(y)(x)"
    case Arg1First => "λrλyλx r(x)(y)"
  }}
}

/* A lexical item that associates question words that have exactly one 
 * argument variable with a relation and argument ordering. For example, this
 * could represent "how many people live in $y" = population(x, y). 
 */
case class QuestionRelItem(words: IndexedSeq[QToken], relation: String, 
    argOrder: ArgOrder) extends LexItem {
  
  if (words.count(_ == RelVar) != 0) throw new 
    IllegalArgumentException("QuestionRelItem cannot contain RelVar: " + words)
  if (words.count(_ == ArgVar) != 1) throw new
    IllegalArgumentException("QuestionRelItem must contain one ArgVar: " + words)
  
  val ws = words.mkString(" ")
  override def toString = s"$ws = " + { argOrder match {
    case Arg2First => s"λyλx $relation(x, y)"
    case Arg1First => s"λyλx $relation(y, x)"
  }}
  
}

/* A span of type T associates some span of tokens with an object of type T
 * (a lexical item).
 */
case class Span[+T](interval: Interval, item: T) {
  def overlaps(that: Span[_]) = this.interval.intersect(that.interval).size > 0
}

/* A derivation represents a combination of lexical items into a query. */
trait Derivation {
  def question: IndexedSeq[QWord]
  def lexItems: IndexedSeq[LexItem]
  def query: SimpleQuery
  
  def qws = question.mkString(" ")
}

/* A two-argument derivation is a derivation that uses a QuestionItem, a
 * RelItem, and an EntItem to derive a query from a question.
 */
case class TwoArgDerivation(
    val question: IndexedSeq[QWord], 
    val questionItem: QuestionItem, 
    val relSpan: Span[RelItem], 
    val entSpan: Span[EntItem]) extends Derivation { 
	  
  def relItem = relSpan.item
  def entItem = entSpan.item
  def lexItems = IndexedSeq(questionItem, relItem, entItem)
  
  val qOrder = questionItem.argOrder
  val rOrder = relItem.argOrder
  val queryField = ArgOrder.compose(qOrder, rOrder)
  
  val query = SimpleQuery(relItem.relation, entItem.entity, queryField)
  
  override def toString = List(qws, query, questionItem, relItem, 
      entItem).mkString("\n")
}

case class WeightedDerivation(
    val question: IndexedSeq[QWord],
    val questionItem: QuestionItem with Weight,
    val relSpan: Span[RelItem with Weight],
    val entSpan: Span[EntItem with Weight],
    val weight: Double)
  extends Derivation with Weight {
  
  def relItem = relSpan.item
  def entItem = entSpan.item
  override def lexItems = IndexedSeq(questionItem, relItem, entItem)
  
  val qOrder = questionItem.argOrder
  val rOrder = relItem.argOrder
  val queryField = ArgOrder.compose(qOrder, rOrder)
  val query = SimpleQuery(relItem.relation, entItem.entity, queryField)
  
  private def weightString = "wt=%.02f rel=%.02f ent=%.02f qst=%.02f"
    .format(weight, relSpan.item.weight, entSpan.item.weight, questionItem.weight)
  
  override def toString = List(qws, weightString, query, "----", questionItem, relItem, 
      entItem).mkString("\n") 
}

/* A one-argument derivation is uses a QuestionRelItem and an EntItem. */
case class OneArgDerivation(question: IndexedSeq[QWord], qrItem: QuestionRelItem, 
    entSpan: Span[EntItem]) extends Derivation {
  val entItem = entSpan.item
  val lexItems = IndexedSeq(qrItem, entItem)
  val queryField = ArgOrder.queryArg(qrItem.argOrder)
  val query = SimpleQuery(qrItem.relation, entItem.entity, queryField)
  override def toString = List(qws, query, qrItem, entItem).mkString("\n")
}

/* A lexicon provides access to a set of lexical items. The lexical items are
 * indexed by the question tokens.
 */
trait Lexicon {
  
  def get(words: IndexedSeq[QToken]): Iterable[LexItem]
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem]
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem]
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem]
  def getQuestionRel(words: IndexedSeq[QToken]): Iterable[QuestionRelItem]
  def has(words: IndexedSeq[QToken]): Boolean
  
  def allQWords(words: IndexedSeq[QToken]): Boolean = 
    words.forall(_.isInstanceOf[QWord]) 
  def toQWords(words: IndexedSeq[QToken]): IndexedSeq[QWord] = 
    words.collect { case w: QWord => w }
}

trait WeightedLexicon extends Lexicon {
  
  def get(words: IndexedSeq[QToken]): Iterable[LexItem with Weight]
  def getRel(words: IndexedSeq[QWord]): Iterable[RelItem with Weight]
  def getEnt(words: IndexedSeq[QWord]): Iterable[EntItem with Weight]
  def getQuestion(words: IndexedSeq[QToken]): Iterable[QuestionItem with Weight]
  def getQuestionRel(words: IndexedSeq[QToken]): Iterable[QuestionRelItem with Weight]
  
  def has(words: IndexedSeq[QToken]): Boolean
}

/* An implementation of a Lexicon represented as a Scala Map object. */
case class MapLexicon(items: Iterable[LexItem]) 
  extends Lexicon {
  
  type QTokens = IndexedSeq[QToken]
  type QWords = IndexedSeq[QWord]
  type LexItems = IndexedSeq[LexItem]
  
  val map = items.groupBy(i => i.words)
  
  def get(words: QTokens) = map.getOrElse(words, IndexedSeq())
  def has(words: QTokens) = map.contains(words)
  def getRel(words: QWords) = get(words) flatMap {
    case x: RelItem => x :: Nil 
    case _ => Nil
  }
  def getEnt(words: QWords) = get(words) flatMap {
    case x: EntItem => x :: Nil
    case _ => Nil
  }
  def getQuestion(words: QTokens) = get(words) flatMap {
    case x: QuestionItem => x :: Nil
    case _ => Nil
  }
  def getQuestionRel(words: QTokens) = get(words) flatMap {
    case x: QuestionRelItem => x :: Nil
    case _ => Nil
  }
  
}

/* A parser maps a question to a set of derivations using a lexicon. */ 
abstract class Parser(val lexicon: Lexicon) {
  
  def intervals(size: Int) =
    for (i <- Range(0, size); j <- Range(i, size)) yield (i, j + 1)

  /* Generates a question pattern like the above, but only for a one-arg
   * quesion pattern.
   */
  def oneArgQuestionPatFrom(words: IndexedSeq[QWord], ent: Span[LexItem]): 
    IndexedSeq[QToken] = {
    val eInt = ent.interval
    (0 until words.size).flatMap { i =>
      if (eInt.min == i) Some(ArgVar)
      else if (!eInt.contains(i)) Some(words(i))
      else None
    }
  }
  
  def rSpan(s: Any): Option[Span[RelItem]] = s match {
    case Span(iv, i: RelItem) => Some(Span(iv, i))
    case _ => None
  }

  def eSpan(s: Any): Option[Span[EntItem]] = s match {
    case Span(iv, i: EntItem) => Some(Span(iv, i))
    case _ => None
  }
    
  /* Gets all non-overlapping relation and entity spans in the question.*/
  def relEntSpans(words: IndexedSeq[QWord]) = {
    val spans = enumerateItemSpans(words)
    val eSpans = spans.flatMap(eSpan)
    val rSpans = spans.flatMap(rSpan)
    for (r <- rSpans; e <- eSpans; if !r.overlaps(e)) yield (r, e)
  }
    
  def enumerateItemSpans(words: IndexedSeq[QWord]): Iterable[Span[LexItem]] = {
    for ((i, j) <- intervals(words.size).par; // for each span index
         ws = words.slice(i, j);		  // get the subseq of words
         iv = Interval.open(i, j);		  // make an interval
         item <- lexicon.get(ws))		  // for each lexical item matching words
      yield Span(iv, item) 			 	  // yield a new Span
    }.toList

  /* Generates a question pattern. For example, suppose we have the input 
   * question "who likes joe" and the lexicon indicated that 
   * "likes" goes to some relation lexitem, and "joe" goes to some entity
   * lexitem. Then this function will return the question pattern "who $r $e".
   */
  def twoArgQuestionPatFrom(words: IndexedSeq[QWord], rel: Span[LexItem],
    ent: Span[LexItem]): IndexedSeq[QToken] = {
    val rInt = rel.interval
    val eInt = ent.interval
    (0 until words.size).flatMap { i =>
      if (rInt.min == i) Some(RelVar)
      else if (eInt.min == i) Some(ArgVar)
      else if (!rInt.contains(i) && !eInt.contains(i)) Some(words(i))
      else None
    }
  }
    
  def parse(words: IndexedSeq[QWord]): Iterable[Derivation]
}

abstract class WeightedParser(override val lexicon: WeightedLexicon) extends Parser(lexicon) {
   
  type RelSpan = Span[RelItem with Weight]
  type EntSpan = Span[EntItem with Weight]
  type LexSpan = Span[LexItem with Weight]
  
  override def rSpan(s: Any): Option[RelSpan] = s match {
    case Span(iv, i: RelItem with Weight) => Some(Span(iv, i))
    case _ => None
  }

  override def eSpan(s: Any): Option[Span[EntItem with Weight]] = s match {
    case Span(iv, i: EntItem with Weight) => Some(Span(iv, i))
    case _ => None
  }
    
  override def relEntSpans(words: IndexedSeq[QWord]): Iterable[(RelSpan, EntSpan)] = {
    val spans = enumerateItemSpans(words)
    val eSpans: Iterable[EntSpan] = spans.flatMap(eSpan)
    val rSpans: Iterable[RelSpan] = spans.flatMap(rSpan)
    for (r <- rSpans; e <- eSpans; if !r.overlaps(e)) yield (r, e)
  }
  
  override def enumerateItemSpans(words: IndexedSeq[QWord]): Seq[LexSpan] = {
    val itemSpans = intervals(words.size).par.flatMap {
      case (start, end) =>
        val wordSlice = words.slice(start, end)
        val sliceInterval = Interval.open(start, end)
        val lexItems = lexicon.get(wordSlice)
        lexItems.map { item => Span(sliceInterval, item) }
    }
    (Seq() ++ itemSpans).sortBy(-_.item.weight)
  }

  override def parse(words: IndexedSeq[QWord]): Iterable[WeightedDerivation]
  
}

/* An implementation of a Parser. It operates by finding all non-overlapping
 * spans of entity and relation items in the question. Then, it checks to see
 * if the resulting question pattern exists in the lexicon.
 */
case class BottomUpParser(override val lexicon: Lexicon) extends Parser(lexicon) {
  
  def entSpans(words: IndexedSeq[QWord]) = 
    enumerateItemSpans(words).flatMap(eSpan)
  
  def parseTwoArg(words: IndexedSeq[QWord]) = {
    for ((r, e) <- relEntSpans(words);
        qpat = twoArgQuestionPatFrom(words, r, e);
        qitem <- lexicon.getQuestion(qpat))
      yield new TwoArgDerivation(words, qitem, r, e)
  }
  
  def parseOneArg(words: IndexedSeq[QWord]) = {
    for (e <- entSpans(words); 
    	qpat = oneArgQuestionPatFrom(words, e);
    	rqitem <- lexicon.getQuestionRel(qpat))
      yield OneArgDerivation(words, rqitem, e)
  }
  
  def parse(words: IndexedSeq[QWord]) = parseTwoArg(words) ++ parseOneArg(words)
}

case class WeightedBottomUpParser(override val lexicon: WeightedLexicon) extends WeightedParser(lexicon) {
  
  def entSpans(words: IndexedSeq[QWord]) = 
    enumerateItemSpans(words).flatMap(eSpan)
  
  def parseTwoArg(words: IndexedSeq[QWord]) = {
    for ((r, e) <- relEntSpans(words);
        qpat = twoArgQuestionPatFrom(words, r, e);
        qitem <- lexicon.getQuestion(qpat))
      yield new WeightedDerivation(words, qitem, r, e, qitem.weight)
  }
  
  /** 
   *  TODO: Extend Weight implementation to handle both kinds of derivation...
   */
  def parseOneArg(words: IndexedSeq[QWord]) = Nil/*{
    for (e <- entSpans(words); 
    	qpat = oneArgQuestionPatFrom(words, e);
    	rqitem <- lexicon.getQuestionRel(qpat))
      yield OneArgDerivation(words, rqitem, e)
  }*/
  
  def parse(words: IndexedSeq[QWord]) = parseTwoArg(words) ++ parseOneArg(words)
}

