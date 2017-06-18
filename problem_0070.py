#!/usr/bin/env python3
"""
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
"""

from common import prime_sieve
import logging
import sys
from time import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname): %(message)')

def main():
    phi_low = ()
    limit = 10**7
    for n, f in factor_gen(limit):
        n_phi = phi(f, n)
        if is_perm(n,n_phi):
            logging.debug("n:{}, phi(n):{}".format(n,n_phi))
            phi_fract = n/n_phi
            if not phi_low or phi_low[2] > phi_fract:
                phi_low = (n,n_phi,phi_fract)
    print("Lowest n:{}, phi(n):{}, n/phi(n):%f".format(phi_low))

def factor_gen(limit):
    limit -= 1
    numbers = [ [] for x in range(limit) ]
    offset = 2
    for i, n in enumerate(numbers):
        if not n:
            for j in range(i,limit,i+offset):
                numbers[j].append(i+offset)
        yield (i+offset, n)

def is_perm(n,m):
    n = sorted([int(x) for x in list(str(n))])
    m = sorted([int(x) for x in list(str(m))])
    if n == m: return True
    else:      return False

def phi(fact, val):
    p = val
    for f in fact:
        p *= 1-1.0/f
    return int(p)

if __name__ == "__main__":
    start = time()
    main()
    logging.info("That took {:4.2} seconds".format(time() - start))
