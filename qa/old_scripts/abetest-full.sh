for qset in {trec,webquestions,wikianswers}; do

JAVA_OPTS=-Xmx16g ./target/start \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=eval/qa/models/abe-2014-02-13-190125/model.1.avg.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    edu.knowitall.eval.qa.QASystemRunner \
    eval/qa/questions/$qset.test.txt \
    eval/qa/output/abe-2014-02-13-190125-model.1.avg.txt-$qset.test

done
