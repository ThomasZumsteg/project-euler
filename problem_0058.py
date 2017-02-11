#!/usr/bin/env python3
"""
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49
It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
"""

from common import prime_sieve
import itertools
import logging
import sys
import time

logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    count = 1
    n_primes = 0
    primes = prime_sieve(100000)
    for s in spiral_gen():
        if s**0.5 > primes[-1]:
            logging.debug("extending {}: {}".format(primes[-1], s))
            primes.extend(prime_sieve(100000, primes))
        if is_prime(s,primes):
            n_primes += 1
            logging.debug("new prime {}".format(s))
        count += 1
        if float(n_primes)/count < 0.1:
            side_len = 2 * ((count + 2)// 4) + 1
            print("Prime ratio is {:5.4f} when the side length is {}".format(
                100 * float(n_primes)/count, side_len))
            return None

def is_prime(num, primes):
    for prime in primes:
        if prime > num**0.5: return True
        if num % prime == 0: return False

def spiral_gen():
    count = 1
    for i in itertools.count():
        for j in range(4):
            count += (i+1)*2
            yield count
            logging.debug("Adding {}".format(count))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
