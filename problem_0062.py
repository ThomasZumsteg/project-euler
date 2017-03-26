#!/usr/bin/env python3
"""
The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
Find the smallest cube for which exactly five permutations of its digits are cube.
"""

import logging
import sys
from time import time
from itertools import count

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format="%(levelname)s:%(message)s")

def main():
    digits = {}
    n = 0
    num_perm = 5
    print_str = "{} has {} cubeic permutations: {}"
    while True:
        n += 1
        sn = digit_hash(n**3)
        add_key_value(digits, sn, n)
        m = n
        limit = int(sn[::-1])**(1.0/3)
        while len(digits[sn]) <= num_perm and m < limit:
            m += 1
            sm = digit_hash(m**3)
            add_key_value(digits, sm, m)

        if len(digits[sn]) == num_perm:
            items = digits[sn]
            print(print_str.format(min(items)**3, len(items), str(items)))
            return

def digit_hash(n):
    return ''.join(sorted(list(str(n))))

def add_key_value(d,k,v):
    try:
        d[k].add(v)
    except KeyError:
        d[k] = set([v])

if __name__ == "__main__":
    start = time()
    main()
    logging.info("That took {:4.2f} seconds".format(time() - start))
