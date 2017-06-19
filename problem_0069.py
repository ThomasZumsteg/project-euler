#!/usr/bin/env python3
"""
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
n   Relatively Prime    φ(n)    n/φ(n)
2   1   1   2
3   1,2     2   1.5
4   1,3     2   2
5   1,2,3,4     4   1.25
6   1,5     2   3
7   1,2,3,4,5,6     6   1.1666...
8   1,3,5,7     4   2
9   1,2,4,5,7,8     6   1.5
10  1,3,7,9     4   2.5

It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.
"""

import logging
import sys
from time import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    num = 10**7
    max_r_primes = None
    max_r = 0
    set_list = sieve(num)
    logging.debug("Sieve Done")
    for n, i_set in enumerate(set_list[1:]):
        phi = n+2
        for i in i_set:
            phi *= (1-1.0/i)
        logging.debug("n:{}, phi:{}, fact: {}".format(n+2,phi,str(i_set)))
        if max_r_primes is None or (n+2)/phi > max_r_primes:
            max_r = n+2
            max_r_primes = (n+2)/float(phi)
    print("{} is {:0.3f} time larger then the number of relatively prime numbers it has".format(max_r, max_r_primes))

def sieve(num):
    set_list = [set() for x in range(num)]
    for i in range(1,len(set_list)):
        logging.debug(i)
        if len(set_list[i]) == 0:
            for j in range(i,num,i+1):
                set_list[j].add(i+1)
    return set_list

if __name__ == "__main__":
    start = time()
    main()
    logging.info("That took {:4.2f} seconds".format(time() - start))
