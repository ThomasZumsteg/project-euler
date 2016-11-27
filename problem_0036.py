#!/usr/bin/env python3
"""
The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
(Please note that the palindromic number, in either base, may not include leading zeros.)
"""

import time
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format="%(levelname)s: %(message)s")

def main():
    pal_sum = 0
    for i in range(1,int(1e6)):
        int_is_pal = is_palindrome(str(i))
        bin_is_pal = is_palindrome(bin(i)[2:])
        if bin_is_pal and int_is_pal:
            pal_sum += i
            logging.debug('This is a palindrome {}: {}'.format(i,bin(i)))
    print('The sum of all palindromes below 1 million is {}'.format(pal_sum))

def is_palindrome(string):
    half_way = len(string)//2
    for a,b in zip(string[:half_way],reversed(string[half_way:])):
        if a != b: return False
    return True

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
