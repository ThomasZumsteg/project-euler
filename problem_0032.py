#!/usr/bin/env python3

"""
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
"""

from itertools import permutations
import logging
from re import sub
import sys
from time import time

logging.basicConfig(level=logging.WARNING, stream=sys.stderr,
    format="[%(levelname)-8s] %(message)s")

def main():
    product_list = []
    for perm in permutations('123456789'):
        order = ''.join(perm)
        logging.debug(order)
        for i in range(1,len(order)-1):
            for j in range(i+1,len(order)):
                a = int(order[:i])
                b = int(order[i:j])
                c = int(order[j:] )
                product = a * b
                if product > c:
                    break
                if product == c:
                    logging.info('{} x {} = {}'.format(a,b,c))
                    product_list.append(product)
    print(sum(list(set(product_list))))

def pair_gen(digits):
    pass

def is_pandigital(digits, n=9):
    for i in range(1,n+1):
        if not digits.count(i) == 1:
            return False
    for i in range(n+1,10):
        if not digits.count(i) == 0:
            return False
    return True

if __name__ == "__main__":
    start = time()
    main()
    print('That took {:.4f} seconds'.format(time() - start))
