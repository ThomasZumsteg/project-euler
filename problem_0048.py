#!/usr/bin/env python3
"""
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
"""

import time
import sys
import logging

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s:%(message)s')

def main():
    num_sum = 0
    for num in reversed(range(1,1001)):
        digits = num**num % int(1e10)
        num_sum += digits
    num_sum = num_sum % int(1e10)
    print('Answer: {}'.format(num_sum))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
