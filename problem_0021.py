#!/usr/bin/env python3

from common import factors_of_num
from time import time

def main():
    amicable_sum = 0
    for num in range(10000):
        if is_amicable(num):
            amicable_sum += num
    print('Answer: {}'.format(amicable_sum))

def is_amicable(num):
    a_pair = sum(factors_of_num(num))
    if a_pair == num: return False
    b_pair = sum(factors_of_num(a_pair))
    return b_pair == num

if __name__ == "__main__":
    start = time()
    main()
    print('That took {:0.04f} seconds'.format(time() - start))
