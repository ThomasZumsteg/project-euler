#!/usr/bin/env python3
"""
All square roots are periodic when written as continued fractions and can be written in the form:
√N = a0 + (1 / a1 + (1 / a2 + (1 / a3 + ...

For example, let us consider √23:
√23 = 4 + √23 — 4 = 4 + 1 = 4 + (1 / (1 / √23—4)) = 4 + (1 / (1 / ((√23 – 3)/ 7)))

If we continue we would get the following expansion:
√23 = 4 + (1 / (1 + (1 / (3 + (1 / (1 + 1 / 8 + ...

The process can be summarised as follows:
a0 = 4, 1/(√23—4) = (√23+4)/7 = 1 + (√23—3)/7
a1 = 1, 7/(√23—3) =  7(√23+3)/14 = 3 + (√23—3)/2
a2 = 3, 2/(√23—3) = 2(√23+3)/14 = 1 + (√23—4)/7
a3 = 1, 7/(√23—4) = 7(√23+4)/7 = 8 + (√23—4)
a4 = 8, 1/(√23—4) = (√23+4)/7 = 1 + (√23—3)/7
a5 = 1, 7/(√23—3) = 7(√23+3)/14 = 3 + (√23—3)/2
a6 = 3, 2/(√23—3) = 2(√23+3)/14 = 1 + (√23—4)/7
a7 = 1, 7/(√23—4) = 7(√23+4)/7 = = 8 + (√23—4)

It can be seen that the sequence is repeating. For conciseness, we use the notation √23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

The first ten continued fraction representations of (irrational) square roots are:

√2=[1;(2)], period=1
√3=[1;(1,2)], period=2
√5=[2;(4)], period=1
√6=[2;(2,4)], period=2
√7=[2;(1,1,1,4)], period=4
√8=[2;(1,4)], period=2
√10=[3;(6)], period=1
√11=[3;(3,6)], period=2
√12= [3;(2,6)], period=2
√13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?
"""

import logging
import sys
import time


logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    odd = 0
    for num in range(10000):
        period = find_period(num)
        if period % 2 == 1: odd += 1
    ps="There are {} coninued fractions with odd periods below 10000"
    print(ps.format(odd))

def find_period(n):
    remainers = []
    digits = []
    for a,rem in const_fract_gen(n):
        try: count = remainers.index(rem)
        except ValueError:
            remainers.append(rem)
            digits.append(a)
        else:
            count = len(remainers) - count
            break
    digits = ''.join([str(n) for n in digits])
    try: count
    except NameError:
        logging.debug(digits)
        return 0
    else:
        logging.debug("[{};({})]".format(
            str(digits[:-count]),
            str(digits[-count:])))
        return count

def const_fract_gen(n):
    m = 0
    d = 1
    a = a0 = int(n**0.5)
    while True:
        logging.debug("\n{} + (sqrt({})-{})/{}".format( a, n, d*a-m, d))
        yield (a,(d,m))
        m = d * a - m
        d = (n - m**2) / d
        try: a = (a0 + m) // d
        except ZeroDivisionError: break

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
