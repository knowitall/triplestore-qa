package edu.knowitall.execution.generalize

import edu.knowitall.triplestore.SolrClient
import edu.knowitall.triplestore.TriplestoreClient
import edu.knowitall.execution.Search.CountQuery
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.QueryExecutor
import edu.knowitall.execution.Search.Field
import edu.knowitall.execution.Search.arg1
import edu.knowitall.execution.Search.arg2
import edu.knowitall.execution.Search.rel
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.TVal
import edu.knowitall.execution.UQuery
import edu.knowitall.execution.UnquotedTLiteral

class FrequencyGeneralizingExecutor(baseExecutor: QueryExecutor, tsClient: TriplestoreClient) extends QueryExecutor {

  def tsFrequency(word: String): Long = {
    val countQuery = CountQuery(word)
    val count = tsClient.count(countQuery)
    count
  } 
  
  def wordFrequencies(words: Seq[String]): Seq[(String, Long)] = words.map(w => (w, tsFrequency(w)))
  
  def maxWordFrequency(words: Seq[String]): Long = {
    if (words.isEmpty) -1L
    else wordFrequencies(words).map(_._2).max
  }
  
  def removeMostFrequent(words: Seq[String]): Seq[String] = {
    val freqs = wordFrequencies(words)
    val mostFrequent = freqs.maxBy(_._2)
    val mostFrequentRemoved = words.filter(_ != mostFrequent._1)
    mostFrequentRemoved
  }
  
  type Phrase = Seq[String]
  
  // Finds the phrase with the most frequent word, and removes the most frequent word from it.
  // caller must check that all phrases is not empty, and no individual phrase is empty.
  def generalizePhrases(phrases: Seq[Phrase]): Seq[Phrase] = {
    
    // annotate phrases with their maxWordFrequency.
    // Unless there is only one word in the phrase, in which case
    // deleting it would make it empty...
    val maxWordFreqs: Seq[(Phrase, Long)] = phrases.map {p =>
      val freq = if (p.size <= 1) -1L else maxWordFrequency(p)
      (p, freq) 
    }
    // find the max word freq of all phrases
    val maxOverAll = maxWordFreqs.map(_._2).max
    // find the index of first phrase with this frequency, in case of ties
    val maxIndex = maxWordFreqs.indexWhere(_._2 == maxOverAll)
    // replace the phrase at this index with a generalization
    val generalizedMaxFreq = maxWordFreqs.zipWithIndex.map { case ((phrase, freq), index) =>
      if (index != maxIndex) phrase
      else removeMostFrequent(phrase)
    }
    generalizedMaxFreq
  }
  
  def generalizeConjuncts(conjuncts: Seq[TConjunct]): Seq[TConjunct] = {
    
    // expand conjunct fields to ((conjunct name, field name) -> field value)
    val expanded = conjuncts.flatMap { c =>
      c.values.map { case (key, value) =>
        ((c.name, key), value)
      }
    }
    // for literal field values, convert to phrases
    val phraseMap = expanded.collect { case ((name, field), UnquotedTLiteral(phrase)) =>
      ((name, field), phrase.split(" ").toSeq)
    }
    
    // get seq of phrases from phraseMap
    val phrases = phraseMap.map(_._2).toSeq
    // generalize
    val generalizedPhrases = generalizePhrases(phrases)
    // zip back with (name, field) pairs
    val genPhraseMap = phraseMap.zip(generalizedPhrases).map { 
      case (((name, field), _), genPhrase) => ((name, field), genPhrase) 
    }.toMap
    // convert back to conjuncts...
    conjuncts.map { c =>
      // update values map
      val newValues = c.values.map { case (key, oldValue) =>
        val phraseLookup = genPhraseMap.get((c.name, key))
        val converted = phraseLookup.map(_.mkString(" ")).map(UnquotedTLiteral(_))
        (key, converted.getOrElse(oldValue))
      }
      c.copy(values = newValues)
    }
  }
  
  def conjunctGeneralizations(conjuncts: Seq[TConjunct]): Iterator[Seq[TConjunct]] = {
    
    val generalization = generalizeConjuncts(conjuncts)
    // stopping criteria: if exists a conjunct with an empty literal field
    val stop = generalization.exists { c =>
      val emptyLiterals = c.values.values.collect({case UnquotedTLiteral("") => ()})
      emptyLiterals.nonEmpty
    }
    if (stop) Iterator.empty
    else (Iterator(generalization) ++ { conjunctGeneralizations(generalization) })
  }
  
  def listQueryGeneralizations(lcq: ListConjunctiveQuery): Iterator[ListConjunctiveQuery] = {
    val conjGeneralizations = conjunctGeneralizations(lcq.conjuncts)
    conjGeneralizations.map(cs => lcq.copy(conjuncts = cs.toList))
  }
  
  type ADs = Iterable[AnswerDerivation]
  
  def deriveAnswers(q: UQuery): ADs = q match {
    case c: ListConjunctiveQuery => { 
      // don't run all of the generalizations - just go until one gets answers, then stop
      val queryResponses = listQueryGeneralizations(c) map baseExecutor.deriveAnswers
      queryResponses.find(_.nonEmpty).getOrElse(Nil)
    }
    case _ => throw new 
      UnsupportedOperationException(s"Unable to execute query type: $q")
  } 
}






