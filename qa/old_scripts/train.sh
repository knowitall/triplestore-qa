JAVA_OPTS="-Xmx8g" ./target/start \
    -Dlearning.numIters=1 \
    -Dparaphrase.template.useTypes=false \
    -Dscoring.weights=src/main/resources/edu/knowitall/search/qa/defaultWeights.txt \
    -Dlearning.oracleMode=file \
    -Dlearning.labelsPath=eval/qa/labels/labels.txt \
    -Dparsing.cg.lexicalRuleKeep='^fullPattern|^.*Identity$' \
    -Dparaphrase.template.maxHits=100 \
    -Dsearch.maxSearchTimeSec=20 \
    -Dlearning.inputsPath=eval/qa/questions/union.train.txt \
    -Dlearning.outputsPath=eval/qa/models \
    -Dlearning.runName=abe \
    edu.knowitall.learning.QaTrainer 
