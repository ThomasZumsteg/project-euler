#!/usr/bin/python
"""http://projecteuler.net/problem=122"""

from time import time
from itertools import combinations_with_replacement as cwr
from sys import stdout

def main():
    limit = 200
    seen = [0,0] + [None] * (limit - 1)
    queue = [[1]]
    while None in seen:
        node = queue.pop(0)
        stdout.write("%d:%d:%d\r" %(seen.count(None),len(node),len(queue)))
        stdout.flush()
        for new_node in node_gen(node):
            if new_node[-1] <= limit:
                if seen[new_node[-1]] == None:
                    seen[new_node[-1]] = len(new_node) - 1
                queue.append(new_node)
    print "\nsum is %d" %(sum(seen))

def node_gen(node):
    lim = node[-1]
    done = set()
    for i,n in reversed(zip(range(1,len(node)+1),node)):
        for m in reversed(node[:i]):
            #print "%d, %d" %(n,m)
            mn = m + n
            if mn <= lim: break
            elif mn not in done:
                yield node + [mn]        
                done.add(mn)

def j_gen(i):
    for j in range(i+1):
        yield 2**j

def slow_main(k):
    nums_seen = [0,0] + [None] * (300-1)
    new = set([frozenset([1])])
    step = 1
    for step in range(1,k+1):
        #stdout.write("%d: %d\r" %(step, nums_seen.count(None)))
        #stdout.flush()
        old = prune(new, step)
        new = set()
        for node in old:
            for (n,new_node) in node_gen(node):
                new.add(new_node)
                try:
                    if nums_seen[n] == None:
                        nums_seen[n] = step
                        #print "%d: %d" %(n,step)
                except IndexError: pass
    see_nums(nums_seen)

def see_nums(nums):
    for s in range(max(nums)):
        string = "%d:" %s
        for i,n in enumerate(nums):
            if n == s:
                string += " %d" %i
        print string

def prune(s,n):
    t = set()
    for i in s:
        if len(i) == n:
            t.add(i)
    return t

#def node_gen(node):
#    for a,b in cwr(node,2):
#        s = frozenset(set([a+b]) | node)
#        yield (a+b, s)

if __name__=="__main__":
    start = time()
    main()
    #for a,s in node_gen(frozenset([1,2])):
    #    print "%d: %s" %(a,str(s))
    print "That took %f seconds" %(time() - start)