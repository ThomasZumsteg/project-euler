#!/usr/bin/python
"""
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on. In this way, we note the following:
    d_2 d_3 d_4  = 406 is divisible by 2
    d_3 d_4 d_5  = 063 is divisible by 3
    d_4 d_5 d_6  = 635 is divisible by 5
    d_5 d_6 d_7  = 357 is divisible by 7
    d_6 d_7 d_8  = 572 is divisible by 11
    d_7 d_8 d_9  = 728 is divisible by 13
    d_8 d_9 d_10 = 289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
"""

import time
from common import lexicographic_gen
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    pan_sum = 0
    for i in lexicographic_gen('0123456789'):
        if has_property(i):
            logging.info("{} has property".format(i))
            pan_sum += int(i)
    print('Answer: {}'.format(pan_sum))

def has_property(num):
    fact_list = [ 2, 3, 5, 7,11,13,17]
    for i in range(1,8):
        mod = int(num[i:i+3]) % fact_list[i-1]
        logging.debug("{} % {} = {}".format(num[i:i+3],fact_list[i-1],mod))
        if int(num[i:i+3]) % fact_list[i-1] != 0:
            return False
    return True

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
