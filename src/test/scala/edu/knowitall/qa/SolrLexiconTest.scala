package edu.knowitall.qa

object SolrLexiconTest extends App {

   val solrLexicon = new SolrLexicon()
   val referenceLexItems = new EvalLexiconLoader(args(0))
   
   referenceLexItems foreach { item =>
     require(solrLexicon.get(item.words).map(_.asInstanceOf[LexItem]).toSet.contains(item), s"Solr doesn't seem to have an entry for: $item")
   }
}