import sys
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
from sys import argv
import getopt
def load_pr(input):
    r,p = [0.0],[1.0]
    #r,p = [],[]
    for line in input:
        try:
            recall,prec = map(str, line.split())
            r.append(recall)
            p.append(prec)
        except:
            break

    d = dict()
    updated_p = []
    prev_r = 0
    for i in range(len(r)):
        p2 = p[i]
        for j in range(i+1, len(r)):
            if p[j] >= p2:
                p2 = p[j]
                d[r[i]] = p[j]
        updated_p.append(p2)

    r = sorted(d)
    updated_p = [d[x] for x in r]
    return r, updated_p

if __name__ == '__main__':

    optlist,args = getopt.getopt(argv[1:], 'o:t:x:y:l:m:')
    opts = dict(optlist)
    
    title = opts.get('-t', '')
    outfile = opts.get('-o', None)
    xlim = float(opts.get('-x', 1.0))
    ylim = float(opts.get('-y', 1.0))
    xlabel = opts.get('-l', 'Recall')
    ylabel = opts.get('-m', 'Precision')

    files = args[0::2]
    names = args[1::2]
    plots = []
    for i,file in enumerate(files):
        r,p = load_pr(open(file))
        plots.append( plt.plot(r, p) )

    plt.xlim(0,xlim)
    plt.ylim(0,ylim)
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.legend( [ p[0] for p in plots ], names )
    if outfile:
        plt.draw()
        plt.savefig(outfile)
    else:
        plt.show()
