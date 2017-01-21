#!/usr/bin/env python3
"""
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
"""

from time import time
import sys
import logging
from common import counter

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s:%(message)s')

def main():
    for i in counter(1):
        digits = sorted(list(str(i)))
        done = True
        for j in range(2,7):
            if not digits == sorted(list(str(i*j))):
                done = False
                break
        if done: break
    print('Answer: {}'.format(i))

if __name__ == "__main__":
    start = time()
    main()
    logging.info("That took {:4.2f} seconds".format(time() - start))
