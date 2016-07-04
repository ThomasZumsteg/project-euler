#!/usr/bin/python
"""http://projecteuler.net/problem=124"""

from time import time
from common import prime_sieve
from sys import stdout

def main():
    big_num = 100000
    primes = prime_sieve(int(big_num**0.5+1))
    # Find distinct prime factors
    nums = [set([x]) for x in range(big_num+1)]
    for n in range(1,big_num+1):
        if n in primes: continue
        for p in primes:
            if n % p == 0:
                nums[n] = set([p]) | nums[n//p]
                break
            elif n**0.5 < p: break
    # Calculate rad from distinct prime factors
    for i, fact in enumerate(nums[1:], start=1):
        nums[i] = (prod(fact), i)
    nums.pop(0)
    # sort
    nums.sort()
    # print
    print "E(10000)=%d" %(nums[10000-1][1])

def prod(items):
    p = 1
    for i in items: p *= i
    return p

if __name__ == "__main__":
    start = time()
    main()
    print "That took %f seconds" %(time() - start)