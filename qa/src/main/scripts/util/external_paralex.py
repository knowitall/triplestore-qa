import sys
from collections import defaultdict
import matplotlib
from numpy import arange
import matplotlib.pyplot as plt

output_file = 'external-paralex.pdf'
title = 'Comparision to external systems'
maxval = 70
width_in = 7
height_in = 2

data_dir = 'eval/qa/output/final'
datasets = ['webquestions', 'trec', 'wikianswers']
systems = ['system-full-uniontrain', 'paralex']
system_names = {
    'system-full-uniontrain': r'\textsc{System}',
    'sempre': r'\textsc{Sempre}',
    'paralex': r'\textsc{Paralex}',
}
dataset_names = {
    'webquestions': 'WebQuestions',
    'trec': 'TREC',
    'wikianswers': 'WikiAnswers'
}
label_position = {
    'system-full-uniontrain': {
        'webquestions': (.28, .39),
        'trec': (.25, .49),
        'wikianswers': (0.059, .20),
    },
    'paralex': {
        'webquestions': (0.16, .25),
        'trec': (0.09, .25),
        'wikianswers': (.027, .13),
    },
    'sempre': {
        'webquestions': (0.35, .57),
        'trec': (0.07, .15),
        'wikianswers': (0.013, .05),
    }
}
system_colors = {
    'system-full-uniontrain': 'blue',
    'sempre': 'red',
    'paralex': 'green',
}
system_linestyles = {
    'system-full-uniontrain': '-',
    'sempre': '-',
    'paralex': '-'
}
system_markers = {
    'system-full-uniontrain': ' ',
    'sempre': ' ',
    'paralex': ' '
}
dataset_recalls = {
    'webquestions': [0, 25, 50],
    'trec': [0, 15, 30],
    'wikianswers': [0, 4, 8]
}



font = {'family' : 'serif',
        'serif'  : 'Computer Modern Roman',
        'size' : 9}
matplotlib.rc('font', **font)
matplotlib.rc('text', usetex=True)

def read_data(dataset, system):
    path = '%s/%s/%s/pr-top.txt' % (data_dir, dataset, system)
    rs, ps = [], []
    for line in open(path):
        r, p = [float(x) for x in line.strip().split()]
        rs.append(r)
        ps.append(p)
    rs, ps = rs[10:], ps[10:]
    n_points = 10
    max_recall = max(rs)
    min_recall = min(rs)
    incr = (max_recall - min_recall) / n_points
    new_r, new_p = [], []
    for i in xrange(n_points + 1):
        recall = min_recall + i*incr
        precision = max(p for (r,p) in zip(rs, ps) if r >= recall)
        new_r.append(recall)
        new_p.append(precision)
    return new_r, new_p

data = defaultdict(dict)
for system in systems:
    for dataset in datasets:
        r, p = read_data(dataset, system)
        data[dataset][system] = (r, p)

fig = plt.figure(figsize=(width_in, height_in))

lines = []
for (i, dataset) in enumerate(datasets):
    ax = fig.add_subplot(1, 3, i+1)
    ax.set_title(dataset_names[dataset], fontsize=9, y=1.0)
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_linewidth(0.5)
    ax.spines['right'].set_color('none')
    ax.spines['bottom'].set_linewidth(0.5)
    if i == 0:
        ax.set_ylabel('Precision')
    ax.set_xlabel('Recall')
    ax.yaxis.set_ticks([0, 0.50, 1.0])
    ax.yaxis.set_ticklabels(['0\\%%', '50\\%%', '100\\%%'])
    all_rs =  []
    for system in systems:
        r, p = data[dataset][system]
        all_rs.extend(r)
        line = ax.plot(r, p, lw=1, color=system_colors[system], linestyle=system_linestyles[system], marker=system_markers[system], markersize=3.5, markeredgecolor='none', markevery=3)
        lines.extend(line)
        lx, ly = label_position[system][dataset]
        ax.text(lx, ly, system_names[system],  fontsize=8, color=system_colors[system])

    ax.set_ylim(0, 1)

    ax.xaxis.set_ticks([ x/100.0 for x in dataset_recalls[dataset]])
    ax.xaxis.set_ticklabels([('%s\\%%' % x) for x in dataset_recalls[dataset]])



names = [system_names[n] for n in systems]
title_text = '{\\bf ' + title + '}'
fig.tight_layout()
plt.savefig(output_file)
