#!/usr/bin/env python3
"""
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
Find the sum of all numbers which are equal to the sum of the factorial of their digits.
Note: as 1! = 1 and 2! = 2 are not sums they are not included.
"""

import time
import logging
import sys

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format="%(levelname)s: %(message)s")

def main():
    factorial_sum = 0
    for i in range(3,2540161):
        factorial = digit_fact(i)
        if i == factorial:
            factorial_sum += i
            logging.info(i)
    print('Answer: {}'.format(factorial_sum))

def digit_fact(num):
    num = list(str(num))
    num_sum = 0
    for i in num:
        num_sum += fact(int(i))
    return num_sum

def fact(num):
    fact_sum = 1
    for i in range(1,num+1):
        fact_sum *= i
    return fact_sum

if __name__ == "__main__":
    start = time.time()
    main()
    print('That took {:4.2f}'.format(time.time() - start))
