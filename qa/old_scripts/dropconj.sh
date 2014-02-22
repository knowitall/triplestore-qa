# union-m1            wiki.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test

# union-m1            trec.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/trec.test.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-trec.test

# union-m1            webq.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/webquestions.test.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-webquestions.test

# shrooms no conj     wiki.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dparsing.cg.maxConjuncts=1 \
    -Dscoring.weights=eval/qa/models/shrooms-2014-02-04-101053/model.5.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.txt \
    eval/qa/output/shrooms-2014-02-04-101053-model.5.avg-wikianswers.test-maxconj1

# sugarcrsip no conj  webq.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dparsing.cg.maxConjuncts=1 \
    -Dscoring.weights=eval/qa/models/sugarcrisp-2014-01-29-192116/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/webquestions.test.txt \
    eval/qa/output/sugarcrisp-2014-01-29-192116-model.1.avg-webquestions.test-maxconj1

# blinkbath no conj   trec.test
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dparsing.cg.maxConjuncts=1 \
    -Dscoring.weights=eval/qa/models/blinkbath-2014-01-31-221632-model.3.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/trec.test.txt \
    eval/qa/output/blinkbath-2014-01-31-221632-model.3.avg-trec.test-maxconj1

# union-m1            wiki.test.extended
JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended


