#!/usr/bin/env python3
"""http://projecteuler.net/problem=38"""

import time
import logging
import sys
from common import counter

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    max_concat = 0
    for i in range(1,10000):
        logging.debug("New {}".format(i))
        concat_num = ''
        for j in counter(1):
            concat_num += str(i*j)
            logging.debug("Testing {}".format(concat_num))
            if len(concat_num) > 9:
                logging.debug("Failed")
                break
            elif len(concat_num) == 9:
                if is_pandigital(concat_num):
                    logging.info("Found pandigital {}: {}".format(i, concat_num))
                    max_concat = max(max_concat, int(concat_num))
    print('Answer: {}'.format(max_concat))

def is_pandigital(string):
    for i in range(1,10):
        if string.count(str(i)) != 1:
            return False
    return True

if __name__ == "__main__":
    start = time.time()
    main()
    print('That took {:4.2f} seconds'.format(time.time() - start))
