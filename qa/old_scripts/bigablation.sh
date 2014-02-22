JAVA_OPTS=-Xmx16g ./target/start \
    -Dsearch.transitions.templateParaphrase=false \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-notemplates

JAVA_OPTS=-Xmx16g ./target/start \
    -Dsearch.transitions.relSyn=false \
    -Dsearch.transitions.isaRelSyn=false \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-norelsyn

JAVA_OPTS=-Xmx16g ./target/start \
    -Dscoring.weights=src/main/resources/edu/knowitall/search/qa/defaultWeights.txt \
    -Dparaphrase.template.useTypes=false \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-defaultWeights

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparsing.cg.maxConjuncts=1 \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-maxconj1

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dtriplestore.skipNamespaces="reverb|openie4" \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-noopenie

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dtriplestore.skipNamespaces="freebase" \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-nofreebase

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dtriplestore.skipNamespaces="probase" \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-noprobase

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dtriplestore.skipNamespaces="nell" \
    -Dscoring.weights=eval/qa/models/debra-2014-02-08-172453/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/wikianswers.test.extended.txt \
    eval/qa/output/debra-2014-02-08-172453-model.1.avg.txt-wikianswers.test.extended-nonell
