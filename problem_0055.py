#!/usr/bin/env python3
"""
If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

How many Lychrel numbers are there below ten-thousand?

NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
"""

import logging
import sys
import time

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    count = 0
    for i in range(10000):
        itter = 1
        for lychrel in lychrel_gen(i):
            if is_palindrome(str(lychrel)):
                logging.debug("{} took {} iters, {}".format(i , itter, lychrel))
                break
            if itter > 50:
                count += 1
                break
            itter += 1
    print("There are {} Lychrel numbers below 10000".format(count))

def lychrel_gen(num):
    while True:
        num = reverse(num) + num
        yield num

def reverse(num):
    string = ''
    for c in reversed(str(num)):
        string += c
    return int(string)

def is_palindrome(string):
    half = len(string) // 2
    for c,d in zip(string[:half],reversed(string[half:])):
        if not c == d: return False
    return True

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
