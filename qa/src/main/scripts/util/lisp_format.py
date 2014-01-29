import sys
for line in sys.stdin:
    astrs, question = line.rstrip('\n').split('\t')
    question = question.replace('"', '\\"')
    if astrs == '-':
        continue
    else:
        answers = astrs.split('|')
    # (example (utterance "How many stores does Wal-Mart operate world-wide?") (targetValues (description "")))
    answers = [a.replace('"', '\\"') for a in answers ]
    descrs = [ '(description "%s")' % a for a in answers ]
    val = '(targetValues (list %s))' % ' '.join(descrs)
    utt = '(utterance "%s")' % question
    ex = '(example %s %s)' % (utt, val)
    print ex
