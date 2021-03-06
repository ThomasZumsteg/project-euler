#!/usr/bin/env python3
"""
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
What 12-digit number do you form by concatenating the three terms in this sequence?
"""

from common import lexicographic_gen
from re import search
import time
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s:%(message)s')

def main():
    primes = prime_sieve(10000)
    found = [1487,4817,8147]
    set_seq = []
    for num in range(1000,10000):
        if search("0",str(num)): continue
        seq = []
        for lex in lexicographic_gen(str(num)):
            if int(lex) in primes: seq.append(int(lex))
        set_seq = is_seq(seq)
        if set_seq and set_seq[0] not in found: break
    print('Answer: {}'.format(''.join([str(x) for x in set_seq])))

def num_set_gen():
    diff   = [1,1,1,1]
    done = False
    while not done:
        the_sum = 0
        string = ''
        for x in diff:
            the_sum += x
            string += str(the_sum)
        yield string
        diff[-1] += 1
        i = len(diff)-1
        while sum(diff) > 9:
            if i-1 < 0:
                done = True
                break
            diff[i] = 1
            diff[i-1] += 1
            i -= 1

def is_seq(seq):
    for i in range(len(seq)-3):
        for j in range(i+1,len(seq)-2):
            diff = seq[j] - seq[i]
            seqk = seq[j] + diff
            if seqk in seq and diff != 0:
                return [seq[i], seq[j], seqk]
    return []

def prime_sieve(limit):
    nums = [ True ] * (limit+1)
    primes = []
    for i in range(2,limit+1):
        if nums[i]:
            primes.append(i)
            for n in range(1,1+limit//i):
                nums[i*n] = False
                logging.debug("{} is not prime".format(i*n))
            logging.debug(str(nums))
    return primes

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time() - start))
