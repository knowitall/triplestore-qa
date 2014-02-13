import sys
from collections import defaultdict
import matplotlib
from numpy import arange
from numpy import std
from numpy import mean
import matplotlib.pyplot as plt

output_file = 'ablation-kb.pdf'
title = 'F1 when components are removed (relative to full system)'
good = 'gray'
bad = 'red'
maxval = 70
width_in = 7
height_in = 1.75

data_dir = 'eval/qa/output/final'
cols = ['webquestions', 'trec', 'wikianswers']
rows = [
    'full',
    'noopenie', 
    'nofreebase', 
    'noprobase', 
    'nonell', 
]
row_names = {
    'full': r'\textsc{System}',
    'noopenie': 'Without Open IE',
    'nofreebase': 'Without Freebase',
    'noprobase': 'Without Probase',
    'nonell': 'Without NELL',
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

def read_data(row, col, default=(0.0, 0.0)):
    path = '%s/%s/system-%s/bootstrap-f1.txt' % (data_dir, col, row)
    try:
        data = [float(x) for x in open(path)]
        f1 = mean(data)
        err = std(data)
        return (f1, err)
    except:
        print >>sys.stderr, 'could not get path: %s' % path
        return default

baseline_values = dict((col, read_data(baseline_row, col)[0]) for col in cols)

f1 = defaultdict(lambda: defaultdict(float))
stds = defaultdict(lambda: defaultdict(float))
for row in rows:
    for col in cols:
        (raw, err) = read_data(row, col, default=(0, 0.0))
        f1[row][col] = raw
        stds[row][col] = err

fig = plt.figure(figsize=(width_in, height_in))

rows.reverse()
for (i, col) in enumerate(cols):
    ax = fig.add_subplot(1, 3, i+1)
    cname = col_names[col]
    ax.set_title(cname, fontsize=9, y=1.0)
    pos = arange(len(rows))
    val = [f1[row][col] for row in rows]
    errs = [stds[row][col] for row in rows]
    colors = [good for row in rows]
    ax.barh(pos, val, align='center', height=.65, color=colors, lw=0) 
    ax.errorbar(val, pos, xerr=errs, elinewidth=0.5, color='black', fmt=' ', capsize=0)
    ax.set_yticks(pos)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_linewidth(0.5)
    ax.spines['left'].set_linewidth(0.5)
    ax.spines['left'].set_color('gray')
    ax.spines['bottom'].set_color('gray')
    ax.xaxis.set_ticks_position('bottom')
    ax.xaxis.set_tick_params(width=0.5, color='gray')
    ax.yaxis.set_tick_params(width=0)
    if i == 0:
        ax.set_yticklabels([row_names[r] for r in rows])
    else:
        ax.set_yticklabels(['' for r in rows])
    maxval = max(abs(v) for v in val)
    maxvalerr = max(abs(v)+e for (v,e) in zip(val,errs))
    ax.set_xlim((0, maxvalerr * 1.1))
    ax.set_xlabel('Test F1', labelpad=-6)
    ax.xaxis.set_ticks([0, baseline_values[col]])
    bval = '%0.2f' % baseline_values[col]
    ax.xaxis.set_ticklabels([0, bval])
    xticks = ax.xaxis.get_major_ticks()
    xticks[0].set_visible(False)
    ax.text(-.005, -1.65, '0')

title_text = '{\\bf ' + title + '}'
fig.tight_layout()
plt.savefig(output_file)
