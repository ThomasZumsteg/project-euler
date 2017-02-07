#!/usr/bin/env python3
"""
It is possible to show that the square root of two can be expressed as an infinite continued fraction.
    √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
By expanding this for the first four iterations, we get:
    1 + 1/2 = 3/2 = 1.5
    1 + 1/(2 + 1/2) = 7/5 = 1.4
    1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
    1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
"""

import logging
import sys
import time

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    i = 1000
    count = 0
    for n, d in root_fract_gen():
        logging.debug("{}/{}".format(n,d))
        if len(str(n)) > len(str(d)): count += 1
        if i < 0: break
        i -= 1
    print("There are {} fraction with the num".format(count))

def root_fract_gen():
    num = 1
    den = 1
    while True:
        (num, den) = (num + 2 * den, num + den)
        yield (num, den)

if __name__ == "__main__":
    start = time.time()
    main()
    print("That took {time:4.2f} seconds" .format(time=time.time() - start))
