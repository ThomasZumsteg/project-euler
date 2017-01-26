#!/usr/bin/env python3
"""
There are exactly ten ways of selecting three from five, 12345:
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
In combinatorics, we use the notation, 5C3 = 10.
In general,
nCr = n!/r!(n−r)!, where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
"""

from time import time
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s:%(message)s')

def main():
    count = 0
    for n in range(1,101):
        for r in range(n):
            combinations = fact(n)/(fact(r)*fact(n-r))
            if combinations > 1e6:
                logging.debug(
                    "There are {} ways of selection {} from {}".format(
                        combinations, r, n))
                count += 1
    print('Answer: {}'.format(count))

def fact(num):
    product = 1
    for i in range(1,num+1):
        product *= i
    return product

if __name__ == "__main__":
    start = time()
    main()
    print("That took {:4.2f} seconds".format(time() - start))
