#!/usr/bin/env python3
"""
A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
"""

import logging
import sys
import time

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    max_digit_sum = 0
    for a in range(100):
        for b in range(100):
            max_digit_sum = max(max_digit_sum, digit_sum(a**b))
    print("Largest digit sum is {max_digit}"
          .format(max_digit=max_digit_sum))

def digit_sum(num):
    sum_digits = 0
    for c in str(num):
        sum_digits += int(c)
    return sum_digits

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {time:4.2f} seconds"
                 .format(time=time.time() - start))
