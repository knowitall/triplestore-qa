import sys
def f1(p, r): return 2*p*r/(p+r)
def get_f1s(d):
    for line in open(d+"/pr-top.txt"):
        r, p = line.strip().split()
        r = float(r)
        p = float(p)
        if r > 1e-8 and p > 1e-8:
            yield f1(p,r)
if __name__ == '__main__':
    ds = sys.argv[1:]
    for d in ds:
        f1 = list(f1 for f1 in get_f1s(d))[-1]
        output = open(d+'/f1-top.txt', 'w')
        print >>output, f1
        output.close()
