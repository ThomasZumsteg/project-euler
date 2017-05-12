#!/usr/bin/env python3
"""
Consider quadratic Diophantine equations of the form:
x² – Dy² = 1
For example, when D=13, the minimal solution in x is 649² – 13×180² = 1.
It can be assumed that there are no solutions in positive integers when D is square.
By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
3² – 2×2² = 1
2² – 3×1² = 1
9² – 5×4² = 1
5² – 6×2² = 1
8² – 7×3² = 1
Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

(x, d, y)
x² - Dy² = 1
(x² - 1) / D = y²
(x² - 1) = D q²
(x² - 1) is a multiple of D and a square
"""

import collections
import logging
import sys
import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    x_max = 0
    d = 0
    for d in range(1,1001):
        logging.debug(d)
        (x, y) = find_min_new(d)
        if not x: continue
        if x > x_max:
            x_max = x
            d_max = d
    print('Answer: {}'.format(d_max))

def find_min(D):
    for y in range(1,100000):
        x = D * (y**2) + 1
        if is_square(x): return (int(x**0.5),y)
        logging.debug("{} - {} * {} = {}".format(int(x**0.5),D,y,x-(D*(y**2))))

def is_square(nn):
    n = int(round((nn)**(0.5)))
    if n**2 == nn: return True
    else: return False

def find_period(n):
    remainers = []
    digits = []
    count = 0
    for a,rem in const_fract_gen(n):
        try: count = remainers.index(rem)
        except ValueError:
            remainers.append(rem)
            digits.append(a)
        else:
            count = len(remainers) - count
            break
    return (digits, count)

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

def const_fract(digits, k, repeat):
    (n2,n1) = (0,1)
    (d2,d1) = (1,0)
    c = 0
    for q in digit_gen(digits, repeat):
        (n2,n1) = (n1,n2 + n1 * q)
        (d2,d1) = (d1,d2 + d1 * q)
        c += 1
        if c > k: return (n1,d1)
        logging.debug("{} / {}".format(n1,d1))

def digit_gen(d, r):
    for i in d[:-r]:
        yield i
    while True:
        for i in d[-r:]:
            yield i

def find_min_new(D):
    (digits, count) = find_period(D)
    if count == 0: return (None, None)
    if count % 2 == 0:
        (x,y) = const_fract(digits, len(digits) - 2, count)
    else:
        (x,y) = const_fract(digits, 2*len(digits) - 3, count)
    return (x,y)

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
