package edu.knowitall.util

import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatizer
import edu.knowitall.repr.sentence.Chunker
import edu.knowitall.tool.tokenize.PTBTokenizer

object NlpTools {
  lazy val tagger = new StanfordPostagger
  lazy val stemmer = new MorphaStemmer
  lazy val tokenizer = new PTBTokenizer
  lazy val chunker = new OpenNlpChunker
  lazy val dummyChunker = DummyChunker(tagger)
  
  def process(sentence: String): Sentence with Chunked with Lemmatized = {
    new Sentence(sentence) with Chunker with Lemmatizer {
      val chunker = NlpTools.dummyChunker
      val lemmatizer = NlpTools.stemmer
    }
  }
}