package edu.knowitall.search.qa

import edu.knowitall.tool.postag.StanfordPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.paraphrasing.template.ParaphraseTemplateClient
import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.triplestore.CachedTriplestoreClient
import edu.knowitall.search.Transition
import edu.knowitall.relsyn.RelSynClient
import edu.knowitall.util.NlpTools

class QaTransitionModel extends Transition[QaState, QaAction] {
  
  lazy val parser = new RegexQuestionParser()
  
  // Remote services
  lazy val templateClient = new ParaphraseTemplateClient
  lazy val baseTriplestoreClient = new SolrClient()
  lazy val triplestoreClient = CachedTriplestoreClient(baseTriplestoreClient)
  lazy val relSynClient = RelSynClient()
  
  // Individual transition functions
  lazy val absArgTransition = new AbstractArgTransition()
  lazy val templateTransition = new TemplateTransition
  lazy val parseTransition = new RegexParseTransition(parser)
  lazy val executeTransition = new ExecutionTransition(triplestoreClient)
  lazy val relSynTransition = new RelSynTransition(relSynClient)
  
  val model = absArgTransition + 
  			  templateTransition +
  			  parseTransition +
  			  relSynTransition +
  			  executeTransition
  			  
  override def apply(s: QaState) = model(s)

}