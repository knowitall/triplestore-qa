# eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt 

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test

JAVA_OPTS=-Xmx16g ./target/start -Dsearch.transitions.templateParaphrase=false -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-notemplates

JAVA_OPTS=-Xmx16g ./target/start -Dsearch.transitions.relSyn=false -Dsearch.transitions.isaRelSyn=false -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-norelsyn

JAVA_OPTS=-Xmx16g ./target/start -Dparaphrase.template.useTypes=false -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-notemplatetypes

JAVA_OPTS=-Xmx16g ./target/start -Dtriplestore.skipNamespaces="reverb|openie4" -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-noopenie

JAVA_OPTS=-Xmx16g ./target/start -Dtriplestore.skipNamespaces="freebase" -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-nofreebase

JAVA_OPTS=-Xmx16g ./target/start -Dtriplestore.skipNamespaces="probase" -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-noprobase

JAVA_OPTS=-Xmx16g ./target/start -Dtriplestore.skipNamespaces="nell" -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.test.txt eval/qa/output/eval/blinkbath-2014-01-31-221632-model.3.avg-trec.test-nonell

