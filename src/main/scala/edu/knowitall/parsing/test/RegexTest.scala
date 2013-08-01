package edu.knowitall.parsing.test

import edu.knowitall.tool.chunk.ChunkedToken
import edu.washington.cs.knowitall.regex
import edu.washington.cs.knowitall.regex.RegularExpression
import edu.washington.cs.knowitall.regex.ExpressionFactory
import edu.washington.cs.knowitall.regex.Expression
import edu.knowitall.tool.chunk.OpenNlpChunker

import scala.collection.JavaConversions._

object RegexTest extends App {

  val expression = 
    "<string='what'> (<rel>:<pos='VB.?' | pos='JJ'>+ <pos='IN' | pos='TO'>) (<ent>:<pos='NN.?'>+)"

  def compile(expr: String) = edu.knowitall.taggers.tag.PatternTagger.makeRegex(expr)
  
  val chunker = new OpenNlpChunker()
  
  val regex = compile(expression)
  
  val sentence = "what is high in protein"
    
  val chunked = chunker.chunk(sentence)
  
  val lemmatized = chunked map edu.knowitall.tool.stem.MorphaStemmer.stemPostaggedToken
  
  println(regex.find(lemmatized).group("ent").tokens.map(_.string))
} 