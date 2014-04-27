#!/usr/bin/python
"""http://projecteuler.net/problem=123"""

from time import time
from common import prime_generator, prime_sieve
from problem120 import square_remainer_gen
from sys import stdout

def main():
    big_num = 10**10
    for n,p in enumerate(prime_generator(block=10000000), start=1):
        if p**2 < big_num: continue
        r = prime_remainder(p,n)
        stdout.write("%d: %d: %d\r" %(n,p,r))
        stdout.flush()
        if r > big_num:
            stdout.write(" " * 40 + "\r")
            print "%d: %d: %d" %(n, p, r)
            return        

def prime_remainder(p,n):
    srg = square_remainer_gen(p)
    for i in range(n-1):
        srg.next()
    return srg.next()
        

if __name__ == "__main__":
    start = time()
    main()
    print "That took %f seconds" %(time() - start)