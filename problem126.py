#!/usr/bin/python
"""http://projecteuler.net/problem=126"""

from time import time
from itertools import count
from sys import stdout

def main():
    big_num = 1000
    tree = {}
    for n in count(1):
        ways = shift_counters(n,tree)
        stdout.write("%d: %d\r" %(n, ways))
        stdout.flush()
        if ways == big_num:
            print "%d is the least value" %(n)
            return
        make_counters(n,tree)

def shift_counters(n,d):
    if n not in d:
        #print "Skipped %d" %(n)
        return 0
    for c,f in enumerate(d[n]):
        v = f.next()
        if v in d:
            d[v].append(f)
        else:
            d[v] = [f]
        #print "Moved to %d" %(v)
    return c + 1

def make_counters(n,d):
    for c,(l,m,n) in enumerate(block_gen(n)):
        f = cube_layer_gen(l,m,n)
        v = f.next()
        if v in d: d[v].append(f)
        else: d[v] = [f]
        #print "Created (%d,%d,%d)=%d" %(l,m,n,v)
    #return c + 1

def cube_layer_gen(l,m,n):
    #yield l*m*n
    center = 2 * (m + n)
    ends = [m*n]
    while True:
        #print "%d: %s" %(center, str(ends))
        yield l * center + 2 * sum(ends)
        ends.append(center)
        center += 4

def block_gen(n):
    # finds ways of making x,y,z such that x*y*z = n
    # given x >= y >= z
    yield (n,1,1)
    for a in range(2,n//2+1):
        if n % a != 0: continue
        for b in range(2,a+1):
            if n % (a * b) != 0: continue
            c = n // (a * b)
            if c <= b: yield (a,b,c)

if __name__ == "__main__":
    start = time()
    main()
    #for i,v in enumerate(cube_layer_gen(1,1,1)):
    #    print "%d: %d" %(i,v)
    #    if i > 4: break
    print "That took %f seconds" %(time() - start)
