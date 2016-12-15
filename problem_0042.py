#!/usr/bin/env python3
"""
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
"""

from re import sub
from math import sqrt
import logging
import sys
import time

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    f_handle = open("problem_0042.txt")
    text = f_handle.read()
    f_handle.close()
    words = text.split(',')
    count = 0
    for word in words:
        logging.debug(word)
        word = sub("[^A-Z]",'',word)
        logging.debug(word)
        word_num = [ord(c)-64 for c in word]
        logging.debug(word)
        word_sum = sqrt(2*sum(word_num)+0.25) - 0.5
        logging.debug(word_sum)
        if word_sum == int(word_sum):
            logging.info("{} is triangle word".format(word))
            count += 1
    print('Answer: {}'.format(count))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
