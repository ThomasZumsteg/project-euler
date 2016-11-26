#!/usr/bin/env python3
"""
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
How many circular primes are there below one million?
"""

from common import prime_generator
from itertools import count
import logging
import sys
import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    prime_gen  = prime_generator()
    prime_list = [next(prime_gen)]
    circular_count = 0
    logging.info("Starting")
    for i in count():
        while i >= len(prime_list):
            logging.debug("Extending primes array")
            new_prime = next(prime_gen)
            prime_list.append(new_prime)
        circular = True
        if prime_list[i] >= 1000000: break
        logging.debug("Testing " + str(prime_list[i]))
        for circ in circ_gen(prime_list[i]):
            while circ > prime_list[-1]:
                prime_list.append(next(prime_gen))
            if circ not in prime_list:
                circular = False
                break
        if circular:
            logging.info("This prime is circular " + str(prime_list[i]))
            circular_count += 1
    print('There are {} circular primes'.format(circular_count))

def circ_gen(number):
    str_num = str(number)
    for i in range(1,len(str_num)):
        yield int(str_num[i:] + str_num[:i])

if __name__ =="__main__":
    start = time.time()
    main()
    print('That took {:4.2f} seconds'.format(time.time() - start))
