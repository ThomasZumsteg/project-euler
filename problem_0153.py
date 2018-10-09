#!/usr/bin/env python3

import logging
import sys
import time
import math
from itertools import count

logging.basicConfig(level=logging.DEBUG)

def main(limit):
    total = 0
    for i in range(0 , 1+int(limit**0.5)):
        for r in range(i+1 if i == 0 else i, 1+int(limit**0.5)):
            square = r**2 + i**2
            for n in range(square, limit+1, square):
                total += (limit // n) * r * (n // square) * (2 if i != 0 else 1)
            logging.debug('({:2},{:2}): {}'.format(r, i, ', '.join(
                '{:2}'.format(n) for n in range(square, limit+1, square))))
    logging.info(total)
    return total

def brute(limit):
    total = 0
    for n in range(1, limit+1):
        ccs = []
        for r in range(1, n+1):
            for i in range(0, n):
                cc = (r**2 + i**2)
                if (n * r) % cc == 0 and (n * i) % cc == 0:
                    gcd = math.gcd(r, i)
                    if i == 0:
                        ccs.append('{}'.format(r))
                        total += r
                    else:
                        ccs.append('{}±{}i'.format(r, i))
                        total += 2 * r
        logging.debug('{}: {}'.format(n, ', '.join(sorted(ccs))))
    return total
                

def test_brute_small():
    assert brute(5) == 35

def test_brute_large():
    assert brute(10**5) == 17924657155

def test_main_small():
    assert main(5) == 35
    for i in range(1, 100):
        assert brute(i) == main(i)

if __name__ == "__main__":
    start = time.time()
    print("Answer: {}".format(main(10**8)))
    print("That took {:4.2f} seconds".format(time.time() - start))
