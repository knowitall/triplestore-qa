#!/bin/bash
#JAVA_OPTS="-Xmx8g" ./target/start -Dlearning.numIters=5 -Dscoring.weights=src/main/resources/edu/knowitall/search/qa/defaultWeights.txt -Dlearning.oracleMode=file -Dlearning.labelsPath=eval/qa/labels/labels.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 -Dlearning.inputsPath=eval/qa/questions/trec.train.txt -Dlearning.outputsPath=eval/qa/models -Dlearning.runName=blinkbath edu.knowitall.learning.QaTrainer &>log.txt

prefix=`ls eval/qa/models/|grep blinkbath|sed 's|/||g'`

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/$prefix/model.1.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.devtest.txt eval/qa/output/$prefix-model.1.avg-trec.devtest 

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/$prefix/model.2.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.devtest.txt eval/qa/output/$prefix-model.2.avg-trec.devtest 

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/$prefix/model.3.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.devtest.txt eval/qa/output/$prefix-model.3.avg-trec.devtest 

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/$prefix/model.4.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.devtest.txt eval/qa/output/$prefix-model.4.avg-trec.devtest 

JAVA_OPTS=-Xmx16g ./target/start -Dscoring.weights=eval/qa/models/$prefix/model.5.avg.txt -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' -Dparaphrase.template.maxHits=100 -Dsearch.maxSearchTimeSec=20 edu.knowitall.eval.qa.QASystemRunner eval/qa/questions/trec.devtest.txt eval/qa/output/$prefix-model.5.avg-trec.devtest 
