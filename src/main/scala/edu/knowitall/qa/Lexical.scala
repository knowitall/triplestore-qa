package edu.knowitall.qa


trait QToken
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
  override def toString = "$e"
}

sealed abstract trait ArgOrder
case object Arg1First extends ArgOrder
case object Arg2First extends ArgOrder

sealed abstract trait Arg
case object Arg1 extends Arg
case object Arg2 extends Arg

case class SimpleQuery(relation: String, entity: String, queryField: Arg) {
  override def toString = queryField match {
    case Arg1 => s"(?, $relation, $entity)"
    case Arg2 => s"($entity, $relation, ?)"
  }
}

trait LexItem

case class EntItem(words: IndexedSeq[QWord], entity: String) extends LexItem {
  val ws = words.mkString(" ")
  override def toString = s"$ws = $entity"
}

case class RelItem(words: IndexedSeq[QWord], relation: String, 
    argOrder: ArgOrder) extends LexItem {
  val ws = words.mkString(" ")
  override def toString = s"$ws = " + { argOrder match {
    case Arg2First => s"λyλx. (x, $relation, y)"
    case Arg1First => s"λyλx. (y, $relation, x)"
  } }
}

    
case class QuestionItem(words: IndexedSeq[QToken], argOrder: ArgOrder) 
	extends LexItem {
   
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

case class QREDerivation(
    question: IndexedSeq[QWord], 
    questionItem: QuestionItem, 
    relItem: RelItem, 
    entItem: EntItem) { 
	  
  val lexItems = IndexedSeq(questionItem, relItem, entItem)
  
  val qws = question.mkString(" ")
  val qOrder = questionItem.argOrder
  val rOrder = relItem.argOrder
  val queryField = (qOrder, rOrder) match {
    case (Arg1First, Arg1First) => Arg1
    case (Arg1First, Arg2First) => Arg2
    case (Arg2First, Arg1First) => Arg2
    case (Arg2First, Arg2First) => Arg1
  }
  
  val query = SimpleQuery(relItem.relation, entItem.entity, queryField)
  
  override def toString = List(qws, query, "----", questionItem, relItem, 
      entItem).mkString("\n")
  
}

trait Lexicon {
  def get(words: IndexedSeq[QToken]): IndexedSeq[LexItem]
  def has(words: IndexedSeq[QToken]): Boolean
}