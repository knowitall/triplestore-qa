from sys import stdin
from sys import argv

# sum reducer

count_index = int(argv[1]) - 1
prev_key = None
nums = []
for line in stdin:
    fields = line.rstrip('\n').split('\t')
    num = float(fields.pop(count_index))
    key = '\t'.join(fields)
    if prev_key is not None and key != prev_key:
        total = sum(nums)
        print '%s\t%s' % (total, prev_key)
        nums = []
    nums.append(num)
    prev_key = key
total = sum(nums)
print '%s\t%s' % (total, prev_key)
