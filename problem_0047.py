#!/usr/bin/env python3
"""
The first two consecutive numbers to have two distinct prime factors are:
14 = 2 × 7
15 = 3 × 5
The first three consecutive numbers to have three distinct prime factors are:
644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.
Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
"""

from common import prime_generator
from itertools import count
import time
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    nums = []
    p_gen = prime_generator()
    primes = [next(p_gen)]
    for i in count(1):
        factors = 0
        while i/30 >= primes[-1]:
            primes.append(next(p_gen))
        for n in primes:
            temp = i
            once = True
            while temp % n == 0:
                if once:
                    factors += 1
                    once = False
                temp /= n
            if i/30 < n or 4 < factors:
                break
        logging.debug('{} has {} factors'.format(i, factors))
        if factors == 4:
            nums.append(i)
        else:
            nums = []
        if len(nums) == 4:
            break
    print('Answer: {}'.format(nums))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
