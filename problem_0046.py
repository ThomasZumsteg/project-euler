#!/usr/bin/env python3
"""
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2
It turns out that the conjecture was false.
What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?"""

from common import prime_generator
import time
import sys
import logging

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s:%(message)s')

def main():
    n = 1
    p_gen = prime_generator()
    prime_list = [next(p_gen)]
    while True:
        n += 2
        while n > prime_list[-1]:
            prime_list.append(next(p_gen))
        if n in prime_list:
            continue
        if not fits_conjecture(n, prime_list):
            print('Answer: {}'.format(n))
            break

def fits_conjecture(num, primes):
    for n in range(1,num):
        i = 0
        while True:
            gold_sum = primes[i] + 2 * n**2
            if gold_sum > num: break
            elif gold_sum == num:
                logging.info('{} = {} + 2 * {}^2'.format(num, primes[i], n))
                return True
            i += 1
    return False

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
