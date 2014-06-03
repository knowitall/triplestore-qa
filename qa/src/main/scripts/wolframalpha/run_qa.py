import wolframalpha
import sys
import time

if len(sys.argv) < 2:
    print >>sys.stderr, "Usage: script api-key < questions"
    exit(-1)

key = sys.argv[1]
client = wolframalpha.Client(key)

for line in sys.stdin:
    try:
        question = line.strip()
        print >>sys.stderr, 'Question: %s' % question
        res = client.query(question)
        pods = res.pods
        if len(pods) >= 2 and pods[1].title == 'Result' and pods[1].text:
            lines = pods[1].text.split('\n')
            if len(lines) > 1: lines = [line.strip() for line in lines if line.strip()]
            for (k, answer) in enumerate(lines):
                if answer == '(data not available)': break
                row = [question, answer, -1 * k, pods[0].text.replace('\n', ' ')]
                print '\t'.join(unicode(x) for x in row)
                print >>sys.stderr, 'Answer: %s' % answer
        else:
            print >>sys.stderr, 'Answer: None'
        time.sleep(2)
    except Exception, e:
        print >>sys.stderr, e
