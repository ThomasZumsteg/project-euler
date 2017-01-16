#!/usr/bin/env python3
"""
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

"""

import logging
import re
import sys
import time

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    pattern_dict = {}
    for prime in prime_generator():
        for pattern in pattern_generator(str(prime)):
            if pattern in pattern_dict:
                pattern_dict[pattern].append(prime)
            else:
                pattern_dict[pattern] = [prime]
            if len(pattern_dict[pattern]) >= 8:
                print('Answer: {}'.format(pattern_dict[pattern][0]))
                return

def pattern_generator(string):
    unique_digits = list(set(string))
    for digit in unique_digits:
        temp =  re.sub(digit,'.',string)
        yield temp
        if temp.count('.') > 1:
            for pattern in sub_pattern(temp,digit):
                yield pattern

def sub_pattern(string,c):
    string = list(string)
    while True:
        for i in range(len(string)):
            if string[i] == c: string[i] = '.'
            elif string[i] == '.':
                string[i] = c
                break
        if '.' in string: yield ''.join(string)
        else: break

def prime_generator(block=100000):
    primes = []
    i = 0
    while True:
        if len(primes) <= i:
            primes = prime_sieve(block, primes)
        yield primes[i]
        i += 1

def prime_sieve(extend,primes=[]):
    logging.debug('Called prime_sieve')
    nums = [ True ] * (extend)
    if not primes: offset = 2
    else:
        offset = primes[-1]+1
        for p in primes:
            start = ( offset - 1 ) // p + 1
            end   = ( offset + extend - 1 ) // p + 1
            for n in range(start, end):
                logging.debug('{} is not prime ({} * {})'.format(n*p, n,p))
                nums[n*p-offset] = False
                logging.debug('Marked {} False'.format(n*p-offset))
    for i in range(extend):
        if nums[i]:
            logging.debug('Found {}'.format(i+offset))
            primes.append(i+offset)
            for n in range(2,1+extend//(i+offset)):
                logging.debug('{} is not prime it {} * {}'.format(
                    (i+offset)*n, n, i+offset))
                nums[(i+offset)*n-offset] = False
    return primes

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info('That took {:4.2f} seconds'.format(time.time()-start))
