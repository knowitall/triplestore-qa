import json
import sys
import re

objs = json.loads(open(sys.argv[1]).read())

pat1 = re.compile(r'\(description "(.*?)"\)')
def parse_target_value(value):
    return pat1.findall(value)
    
for obj in objs:
    question = obj['utterance']
    target = obj['targetValue']
    answers = parse_target_value(target)
    for a in answers:
        print "LABEL\t1\t%s\t%s" % (question, a)
