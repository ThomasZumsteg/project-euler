#!/usr/bin/env python3
"""A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2  =   0.5
    1/3  =   0.(3)
    1/4  =   0.25
    1/5  =   0.2
    1/6  =   0.1(6)
    1/7  =   0.(142857)
    1/8  =   0.125
    1/9  =   0.(1)
    1/10 =   0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part."""

from time import time

def main():
    print('Answer: {}'.format(max(range(2,1000), key=lambda m: div_cycle(1,m))))

def div_cycle(num,denom):
    remainer  = num
    digits    = []
    remainers = []
    while remainer:
        digit, remainer = divmod(10*remainer,denom)
        itter = zip(reversed(digits), reversed(remainers))
        for count, (val, rem) in enumerate(itter):
            if val == digit and rem == remainer:
                return count
        remainers.append(remainer)
        digits.append(digit)
    return 0

if __name__ == '__main__':
    start = time()
    main()
    print('That took {:4.2} seconds'.format(time() - start))
