#!/bin/bash
set -u
set -e
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. $THIS_DIR/../util/get_config.sh
class="edu.knowitall.paraphrasing.template.TemplateCounterStreamingJob"
if [[ -z `set | grep ^hadoop_streaming_jar=` ]]; then
    echo "Required \$hadoop.streaming.jar variable not set."
    echo "Set it in $PROJECT_ROOT/src/main/resources/application.conf"
    exit -1
fi
$START_SCRIPT $class
