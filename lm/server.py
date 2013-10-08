import web
import sys
import kenlm
import json

if len(sys.argv) < 3:
    print >>sys.stderr, "Usage: server.py port lm-path"

lm_path = sys.argv[2]

sys.stderr.write("Loading language model from %s..." % lm_path)
lm = kenlm.LanguageModel(lm_path)
sys.stderr.write("Done.\n")

urls = ('/score', 'score')

app = web.application(urls, globals())

class score:

    def get_scores(self, queries):
        return [lm.score(q) for q in queries]

    def GET(self):
        i = web.input()
        queries = [q.strip() for q in i.q.split('|')]
        print >>sys.stderr, "queries:\n%s" % str('\n'.join(queries))
        return '\n'.join('%0.4f' % s for s in self.get_scores(queries))

    def POST(self):
        return self.GET()

if __name__ == '__main__':
    app.run()
