#!/usr/bin/env python3
"""
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
"""

from common import prime_generator
import time
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    prime_list = []
    trunc_prime_sum = 0
    trunc_prime_count = 0
    for prime in prime_generator():
        prime_list.append(prime)
        if prime < 10:
            continue
        logging.debug("Checking prime {}".format(prime))
        trunc_prime = True
        for i in range(1,len(str(prime))):
            left  = int(str(prime)[:i])
            right = int(str(prime)[i:])
            logging.debug("Testing {} and {}".format(left, right))
            if left not in prime_list or right not in prime_list:
                logging.debug("Nope!")
                trunc_prime = False
                break
        if trunc_prime:
            trunc_prime_count += 1
            logging.info(prime)
            trunc_prime_sum += prime
        if trunc_prime_count >= 11:
            break
    print('Answer: {}'.format(trunc_prime_sum))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
