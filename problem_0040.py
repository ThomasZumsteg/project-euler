#!/usr/bin/env python3
"""
An irrational decimal fraction is created by concatenating the positive integers:
0.12345678910>1<112131415161718192021...
It can be seen that the 12th digit of the fractional part is 1.
If dn represents the nth digit of the fractional part, find the value of the following expression.
d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
"""

import time
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    counter = 1
    string = ''
    while len(string) < 1000010:
        string += str(counter)
        logging.debug(string)
        counter += 1
    product = 1
    logging.info(string[11:14])
    for i in [1,10,100,1000,10000,100000,1000000]:
        logging.info(string[i-1])
        product *= int(string[i-1])
    print('Answer: {}'.format(product))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
