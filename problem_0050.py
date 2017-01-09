#!/usr/bin/env python3
"""
The prime 41, can be written as the sum of six consecutive primes:
    41 = 2 + 3 + 5 + 7 + 11 + 13
    This is the longest sum of consecutive primes that adds to a prime below one-hundred.
    The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
    Which prime, below one-million, can be written as the sum of the most consecutive primes?
"""

import logging
import sys
import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s:%(message)s')

def main():
    block = 100000
    primes = prime_sieve(block)
    i = 0
    max_i = 0
    max_prime_set = 0
    while True:
        i += 1
        if i >= len(primes):
            primes = prime_sieve(block,primes)
        if primes[i] > 1000000: break
        logging.debug("Testing {}".format(primes[i]))
        prime_set = is_prime_sum(primes[i],primes, max_prime_set)
        if prime_set and max_prime_set < len(prime_set):
            max_prime_set = len(prime_set)
            max_i = i
            logging.debug("{} is the sum of {} primes".format(
                primes[i], max_prime_set))
    print("{} is the sum of {} primes".format(primes[max_i], max_prime_set))

def is_prime_sum(num,primes,sum_min):
    i = 0
    while True:
        prime_sum = primes[i:i+sum_min]
        if sum(prime_sum) > num: return False
        j = i + sum_min
        while True:
            prime_sum.append(primes[j])
            if sum(prime_sum) > num: break
            elif sum(prime_sum) == num: return prime_sum
            j += 1
        i += 1

def prime_sieve(extend,primes=[]):
    logging.debug("Called prime_sieve")
    nums = [ True ] * (extend)
    if not primes: offset = 2
    else:
        offset = primes[-1]+1
        for p in primes:
            start = ( offset - 1 ) // p + 1
            end   = ( offset + extend - 1 ) // p + 1
            for n in range(start, end):
                logging.debug("{} is not prime ({} * {})".format(n*p, n,p))
                nums[n*p-offset] = False
                logging.debug("Marked {} False".format(n*p-offset))
    for i in range(extend):
        if nums[i]:
            logging.debug("Found {}".format(i+offset))
            primes.append(i+offset)
            for n in range(2,1+extend//(i+offset)):
                logging.debug("{} is not prime it {} * {}".format(
                (i+offset)*n, n, i+offset))
                nums[(i+offset)*n-offset] = False
    logging.debug(nums)
    return primes

def prime_generator(block=100000):
    primes = []
    i = 0
    while True:
        if len(primes) <= i:
            primes = prime_sieve(block, primes)
        yield primes[i]
        i += 1

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
