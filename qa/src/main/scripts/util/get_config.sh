#!/bin/bash
set -u
set -e
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT=`readlink -m "$THIS_DIR/../../../../"`
conf="$PROJECT_ROOT/src/main/resources/application.conf"

readConf() {
    grep '^[^ ]* *= *[^ ]*$' $conf | grep -v '^ *#' | \
     sed 's/^\(.*\) *= *"*\([^"]*\)"* *$/\1	\2/g' 
}
while read key val; 
    do
        new_key=`echo $key | tr '.' '_'`
        export $new_key="$val"
done < <(readConf)

export START_SCRIPT="$PROJECT_ROOT/target/start"
if [ ! -f $START_SCRIPT ]; then
    echo "Warning: start script not found in '$START_SCRIPT'"
    echo "To create start script, run 'sbt start-script'"
fi

requireConfVar() {
    if [[ -z `set | grep ^$1=` ]]; then                           
        echo "Required '$1' variable not set."                    
        echo "Set it in $PROJECT_ROOT/src/main/resources/application.conf"          
        exit -1                                                                     
    fi 
}