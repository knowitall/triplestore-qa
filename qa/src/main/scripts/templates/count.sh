#!/bin/bash
set -u
set -e
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. $THIS_DIR/../util/get_config.sh

mapperClass="edu.knowitall.paraphrasing.template.TemplateCounterStreamingJob"

if [ $# -ne 2 ]; then
    echo "Usage: $0 hdfs-input hdfs-output"
    exit -1
fi;
input=$1
output=$2
jobName="template counter"
requireConfVar "hadoop.streaming.jar"
requireConfVar "python"
requireConfVar "templates.reducers"
requireConfVar "templates.minfreq"

map="$START_SCRIPT $mapperClass"
reduce="$python $PROJECT_ROOT/src/main/scripts/util/aggsum.py 2 $templates_minfreq"
hadoop jar ${hadoop_streaming_jar} -Dmapred.reduce.tasks="${templates_reducers}" -Dmapred.job.name="${jobName}" -input "${input}" -output "${output}" -mapper "${map}" -reducer "${reduce}"
