package edu.knowitall.wikianswers
import java.net.URLDecoder
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.postag.PostaggedToken

case class WikiAnswersDoc(filename: String, content: String, docType: String) {
  override def toString = s"$filename\t${content.replaceAll("\t", " ")}\t$docType"
}
case object WikiAnswersDoc {
  
  val ALTERNATES_DOC = "alternates"
  val CHANGES_DOC = "changes"
  val QUESTION_DOC = "question"
  
  type QuestionPair = (String, String)
  
  def lineToDoc(line: String): Option[WikiAnswersDoc] = {
    line.split(" ", 2) match {
      case Array(a: String, b: String) => Some(inferDocType(a, b))
      case _ => None
    }
  }
  
  def fromString(s: String): Option[WikiAnswersDoc] = {
    s.split("\t", 3) match {
      case Array(f: String, c: String, d: String) => Some(WikiAnswersDoc(f, c, d))
      case _ => None
    }
  }
  
  def inferDocType(filename: String, content: String): WikiAnswersDoc = {
    if (filename.contains("editalternates")) {
      WikiAnswersDoc(filename, content, ALTERNATES_DOC)
    } else if (filename.contains("Special:Changes")) {
      WikiAnswersDoc(filename, content, CHANGES_DOC)
    } else {
      WikiAnswersDoc(filename, content, QUESTION_DOC)
    }
  }
  
  val titlePat = "<title>([^<]+)</title>".r
  def getTitle(doc: WikiAnswersDoc): Option[String] = {
    titlePat.findFirstIn(doc.content) match {
      case Some(titlePat(title)) => Some(qmark(title))
      case _ => None
    }
  }
  
  val alternatePat = "10px;'>([^&<>]+)\\s+&nbsp;|&nbsp;\\s+<A HREF".r
  def getAlternates(doc: WikiAnswersDoc): Iterable[QuestionPair] = {
    val qs = alternatePat.findAllIn(doc.content).flatMap { 
      case alternatePat(x) => Some(x)
      case _ => None
    }.toList
    for (q1 <- qs; q2 <- qs; if q1 != q2; if q1 != null; if q2 != null) 
      yield (qmark(q1), qmark(q2))
  }
  
  val linkPat = "<b><a [^>]*>([^<]*)</a></b>"
  val changePats = List(
      s"merged the question $linkPat into $linkPat".r, 
      s"$linkPat and said it was the same as $linkPat".r)
  def getChanges(doc: WikiAnswersDoc): Iterable[QuestionPair] = {
    for (
        p <- changePats;
        (x,y) <- p.findAllIn(doc.content).flatMap {
          case p(x, y) => Some(x, y)
          case _ => None
        }.toList;
        (q1, q2) <- List((x,y), (y,x))) yield (qmark(q1), qmark(q2))
        
  }
  
  def stripHtml(s: String) = 
    s.replaceAll("""<[^>]*>""", " ").replaceAll("\\s+", " ")
  
  val ansPat = "<div class=\"answer_text\" id=\"editorText\">(.*?)</div>".r
  def getMainAnswer(doc: WikiAnswersDoc): Option[String] = doc.docType match {
    case QUESTION_DOC => ansPat.findFirstIn(doc.content) match {
      case Some(ansPat(ans)) => Some(stripHtml(ans))
      case _ => None
    }
    case _ => None
  }
  
  def cleanQuestion(q: String): Option[String] = try { 
    URLDecoder.decode(q) match {
    	case s if s.startsWith("FAQ") => None
    	case s if s.startsWith("NEWQ") => None
    	case s if s.startsWith("Special") => None
    	case s if s.startsWith("User") => None
    	case s => Some(s.replaceAll("_", " ") + "?")
    }
  } catch {
    case e: IllegalArgumentException => Some(q)
  }
  
  val qPat = """http://wiki.answers.com/Q/([^&"]+)""".r
  def getQuestions(doc: WikiAnswersDoc): List[String] = {
    for (s <- qPat.findAllIn(doc.content).flatMap {
        	case qPat(s) => Some(s)
        	case _ => None
        };
        q = cleanQuestion(s)) yield q
  }.flatten.toList
  
  def flattenPairs(pairs: Iterable[QuestionPair]) = pairs.map({
    (a: String, b: String) => List(a, b)
  }.tupled).flatten  
  
  def getAllQuestions(doc: WikiAnswersDoc): List[String] = {
    val q1 = getQuestions(doc)
    val q2 = doc.docType match {
      case ALTERNATES_DOC => flattenPairs(getAlternates(doc))
      case CHANGES_DOC => flattenPairs(getChanges(doc))
      case _ => List()
    }
    (q1 ++ q2).distinct
  }
  
  def getQuestionAnswers(doc: WikiAnswersDoc): List[QuestionAnswer] = {
    val question = getTitle(doc)
    val answer = doc.docType match {
      case QUESTION_DOC => getMainAnswer(doc)
      case _ => None
    }
    for (q <- question.toList; a <- answer) yield QuestionAnswer(q, a)
  } 
  
  def qmark(s: String) = s match {
    case s if s.endsWith("?") => s
    case s => s + "?"
  }
  
  def getQuestionParaphrases(doc: WikiAnswersDoc): List[(String, String)] = {
    val paras = doc.docType match {
      case ALTERNATES_DOC => getAlternates(doc)
      case CHANGES_DOC => getChanges(doc)
      case _ => List.empty
    }
    paras.toList.distinct
  }
  
}

trait Keyed {
  val key: String
}

case class ParaphraseCluster(questions: List[String]) extends Keyed {
  val key = questions match {
    case head :: rest => head
    case _ => throw new IllegalArgumentException("questions can't be empty")
  }
  override val toString = questions.mkString("\t")
}

case class QuestionAnswer(question: String, answer: String) extends Keyed {
  val key = question
  override val toString = s"$question\t$answer"
}