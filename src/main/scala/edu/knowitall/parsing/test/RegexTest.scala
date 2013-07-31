package edu.knowitall.parsing.test

import edu.knowitall.tool.chunk.ChunkedToken
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.tool.chunk.OpenNlpChunker

import scala.collection.JavaConversions._

object RegexTest extends App {

  val expression = """ (?:<string="a"> | <string="an"> | <string="the">)? <pos="JJ">* (<arg1>:<pos='NNP'>+) (<rel>:<pos='NN'>+) (<arg2>:<pos='NNP'>+)"""

  def compile(expr: String) = edu.knowitall.taggers.tag.PatternTagger.makeRegex(expr)
  
  val chunker = new OpenNlpChunker()
  
  val regex = compile(expression)
  
  val sentence = "The US president Barack Obama is travelling to Mexico."
    
  val chunked = chunker.chunk(sentence)
  
  val lemmatized = chunked map edu.knowitall.tool.stem.MorphaStemmer.stemPostaggedToken
  
  println(regex.find(lemmatized).group("arg1").tokens.map(_.string))
  
  println(regex.find(lemmatized).group("rel").tokens.map(_.string))
  
  println(regex.find(lemmatized).group("arg2").tokens.map(_.string))
  
} 