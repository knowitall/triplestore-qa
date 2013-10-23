import sys
import urllib2
import urllib
import json
url = 'http://localhost:8083/parse?'
for line in sys.stdin:
    sent = line.strip()
    data = urllib.urlencode(dict(sent=sent))
    resp = urllib.urlopen(url + data)
    results = json.loads(resp.read())
    for x in results['result']:
        score = x['score']
        answers = x['answers']
        query = x['query']
        for a in answers:
            print '%s\t%s\t%s\t%s' % (sent, a, score, query)
