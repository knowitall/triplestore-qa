package edu.knowitall.parsing.test

import edu.knowitall.tool.chunk.ChunkedToken
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.tool.chunk.OpenNlpChunker

import scala.collection.JavaConversions._

object RegexTest extends App {

    val VERB =
        // Optional adverb
        "<pos='RB'>? " +
        // Modal or other verbs
        "<pos='VB' | pos='VBP' | pos='VBZ' | pos='VBG' | pos='VBN'> " +
        // Optional particle/adverb
        "<pos='RP'>? <pos='RB'>?";

    val WORD =
        "<pos='$' | pos='PRP$' | pos='CD' | pos='DT' | pos='JJ' | pos='JJS' | pos='JJR' | pos='NN' " +
        "| pos='NNS' | pos='NNP' | pos='NNPS' | pos='POS' | pos='PRP' | pos='RB' | pos='RBR' | pos='RBS' " +
        "| pos='VBN' | pos='VBG'>";

    val PREP = "<pos='RB'>? <pos='IN' | pos='TO' | pos='RP'> <pos='RB'>?";


    val longRelPattern =
        String.format("(<rel>:%s (?:%s* (?:%s)+)?)+", VERB, WORD, PREP);

    val shortRelPattern =
        String.format("(<rel>:%s (?:%s)?)+", VERB, PREP);
    
    val entityPattern = s"(<ent>:(?:$WORD*))"
    
    val question1 = s"<string='what'> $longRelPattern $entityPattern"
    val question2 = s"<string='what'> <string='did'> $entityPattern $longRelPattern"
    

  def compile(expr: String) = edu.knowitall.taggers.tag.PatternTagger.makeRegex(expr)
  
  val chunker = new OpenNlpChunker()
  
  val regex = compile(question2)
  
  val sentence = "what did thomas edison invent?"
    
  val chunked = chunker.chunk(sentence)
  
  val lemmatized = chunked map edu.knowitall.tool.stem.MorphaStemmer.stemPostaggedToken
  
  println(regex.find(lemmatized).group("rel").tokens.map(_.string))
  println(regex.find(lemmatized).group("ent").tokens.map(_.string))
} 