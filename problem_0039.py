#!/usr/bin/env python3
"""
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
{20,48,52}, {24,45,51}, {30,40,50}
For which value of p ≤ 1000, is the number of solutions maximised?
"""

import time
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    format='%(levelname)s:%(message)s',
    level=logging.INFO)

def main():
    max_int_tri = (0,0)
    for i in range(5,1001):
        int_count = int_right_tri(i)
        if max_int_tri[0] < int_count:
            max_int_tri = (int_count, i)
            logging.info(
                'New max:\n\tperimeter={:4d}\n\tsolutions={:4d}'
                .format(*max_int_tri))
    print('Answer: {}'.format(max_int_tri[1]))

def int_right_tri(perim):
    count = 0
    for a in range(1,int(perim/3)+1):
        b = a
        while b < perim-(a+b):
            c = perim-(a+b)
            logging.debug("Testing: {} {} {}".format(a,b,c))
            if a**2 + b**2 == c**2:
                count += 1
                logging.debug("Set: {}^2 + {}^2 = {}^2".format(a,b,c))
            b+=1
    return count

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f}'.format(time.time() - start))
