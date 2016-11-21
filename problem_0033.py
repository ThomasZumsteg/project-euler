#!/usr/bin/env python3
"""
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
"""

import time
import logging
import sys

logging.basicConfig(level=logging.CRITICAL, stream=sys.stderr)

def main():
    numerator   = 1
    denominator = 1
    for numer in range(10,99):
        for denom in range(numer+1,100):
            for n,d in remove_duplicate_digits(numer, denom):
                if n * denom == d * numer:
                    logging.info('{}/{} = {}/{}'.format(n,d,numer,denom))
                    numerator   *= n
                    denominator *= d
    d = gcd(numerator, denominator)
    print('Answer: {:n}'.format(denominator/d))

def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def remove_duplicate_digits(a,b):
    [a,b] = [str(a), str(b)]
    for i in range(len(a)):
        if a[i] == '0': continue
        for j in range(len(b)):
            if a[i] == b[j]:
                dup_a = int(a[:i] + a[i+1:])
                dup_b = int(b[:j] + b[j+1:])
                if not (dup_a == 0 or dup_b == 0):
                    yield [dup_a, dup_b]


if __name__ == "__main__":
    start = time.time()
    main()
    print('That took {:4.2f} seconds'.format(time.time() - start))
