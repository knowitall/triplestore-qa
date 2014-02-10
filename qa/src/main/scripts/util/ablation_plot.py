import sys
from collections import defaultdict
import matplotlib
from numpy import arange
import matplotlib.pyplot as plt

output_file = 'ablation.pdf'
title = 'F1 when components are removed (relative to full system)'
good = '#6CA0DC'
bad = 'red'
maxval = 70
width_in = 7
height_in = 2.5

data_dir = 'eval/qa/output/final'
cols = ['webquestions', 'trec', 'wikianswers']
rows = [
    'title1',
    'noopenie', 
    'nofreebase', 
    'noprobase', 
    'nonell', 
    '',
    'title2',
    'defaultweights',
    'noparaphrases',
    'maxconj1', 
    'noqueryrewrites', 
    '',
]
row_names = {
    'noopenie': 'Open IE',
    'nofreebase': 'Freebase',
    'noprobase': 'Probase',
    'nonell': 'NELL',
    'noparaphrases': 'Paraphrases',
    'noqueryrewrites': 'Query Rewrites',
    'defaultweights': r'Weight Learning',
    'maxconj1': 'Parser Patterns with Joins',
    '': '',
    'title1': r'\underline{\bf Ablated Knowledge Base}',
    'title2': r'\underline{\bf Ablated Model Component}'
}
col_names = {
    'webquestions': 'WebQuestions',
    'trec': 'TREC',
    'wikianswers': 'WikiAnswers'
}
baseline_row = 'full'

font = {'family' : 'serif',
        'serif'  : 'Computer Modern Roman',
        'size' : 9}
matplotlib.rc('font', **font)
matplotlib.rc('text', usetex=True)
#matplotlib.rc(params)

def read_data(row, col, default=0.0):
    path = '%s/%s/system-%s/f1-top.txt' % (data_dir, col, row)
    try:
        return float(open(path).read())
    except:
        print >>sys.stderr, 'could not get path: %s' % path
        return default

baseline_values = dict((col, read_data(baseline_row, col)) for col in cols)
def relative_change(value, col):
    x = baseline_values[col]
    #return 100 * (value - x) / x
    return value - x

data = defaultdict(lambda: defaultdict(float))
for row in rows:
    for col in cols:
        raw = read_data(row, col, default=baseline_values[col])
        rel = relative_change(raw, col)
        data[row][col] = rel

print '\t'.join(['system'] + cols)
for row in rows:
    vals = [data[row][col] for col in cols]
    print '%s\t%s' % (row, '\t'.join(str(x) for x in vals))

fig = plt.figure(figsize=(width_in, height_in))

rows.reverse()
for (i, col) in enumerate(cols):
    ax = fig.add_subplot(1, 3, i+1)
    cname = col_names[col]
    ax.set_title(cname, fontsize=9, y=1.0)
    pos = arange(len(rows))
#    pos = [ x + (1.0 if k > 3 else 0) for (k,x) in enumerate(pos) ]
    print pos
    val = [data[row][col] for row in rows]
    colors = [good if data[row][col] < 0 else bad for row in rows]
    ax.barh(pos, val, align='center', height=.75, color=colors, lw=0)
    ax.set_yticks(pos)
    ax.spines['top'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_linewidth(0.5)
    ax.xaxis.set_ticks_position('bottom')
    ax.xaxis.set_tick_params(width=0.5)
    ax.yaxis.set_tick_params(width=0)
    if i == 0:
        #ax.yaxis.set_ticks(pos, tuple(row_names[r] for r in rows))
        ax.set_yticklabels([row_names[r] for r in rows])
    else:
        ax.set_yticklabels(['' for r in rows])
    ax.axvline(0, ymin=0, ymax=.90, color='k', lw=0.5)
    maxval = max(abs(v) for v in val)
    ax.set_xlim((-maxval, maxval))
    ax.set_xlabel('F1')
    ax.xaxis.set_ticks([-maxval, 0, maxval])
    bval = '%0.2f' % baseline_values[col]
    ax.xaxis.set_ticklabels(['%0.2f' % (baseline_values[col]-maxval), bval, '%0.2f' % (maxval + baseline_values[col])])

title_text = '{\\bf ' + title + '}'
#fig.text(0.55, 0.05, title_text, horizontalalignment='center')
fig.tight_layout()
#fig.subplots_adjust(bottom=0.2)
plt.savefig(output_file)
