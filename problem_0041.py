#!/usr/bin/env python3
"""
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
What is the largest n-digit pandigital prime that exists?
"""

from common import pandigital_len, prime_generator
import time
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    max_pan_len = (0,0)
    for prime in prime_generator():
        if prime > 987654321: break
        pan_len = pandigital_len(str(prime))
        if pan_len > max_pan_len[0]:
            max_pan_len = (pan_len, prime)
            logging.info(max_pan_len)
    print('Answer: {}'.format(max_pan_len))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
