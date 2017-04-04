#!/usr/bin/env python3

"""
The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
How many n-digit positive integers exist which are also an nth power?
"""

import logging
import sys
import time
from itertools import count

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    matches = 0
    for n in count(1):
        for m in reversed(range(1,10)):
            num_digits = len(str(m**n))
            if num_digits == n:
                logging.debug( "{} = {}^{}".format(m**n, m,n))
                matches += 1
            elif num_digits < n: break
        if m >= 9: break
    ps = "There are {} numbers for which the nth power has n digits"
    print(ps.format(matches))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
