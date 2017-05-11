#!/usr/bin/env python3
"""
The square root of 2 can be written as an infinite continued fraction.
√2 = 1 + (1/(2 + (1/(2 + (1/(2 + (1/(2 + ...))))))))
The infinite continued fraction can be written, √2 = [1;(2)], (2) indicates that 2 repeats ad infinitum. In a similar way, √23 = [4;(1,3,1,8)].
It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for √2.
1 + (1/2) = (3/2)
1 + (1/(2+(1/2))) = (7/5)
1 + (1/(2+(1/(2+(1/2))))) = 17/12
1 + (1/(2+(1/(2+(1/(2+(1/2))))))) = 41/29
Hence the sequence of the first ten convergents for √2 are:
1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
What is most surprising is that the important mathematical constant,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
The first ten terms in the sequence of convergents for e are:
2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
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
