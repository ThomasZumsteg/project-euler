#!/usr/bin/python
"""http://projecteuler.net/problem=125"""

from time import time
from sys import stdout
from itertools import combinations, count

def main():
    num_sum = 0
    big_num = 10**8
    seen = set()
    # generate consecutive square sums
    for n in consec_square_sums(big_num):
        if is_palindrome(n) and n not in seen:
            #print n
            seen.add(n)
            stdout.write("%d: %d\r" %(num_sum, n))
            stdout.flush()
            num_sum += n
    print "Sum of all below %d is %d" %(big_num, num_sum)

def consec_square_sums(lim):
    for m in count(2):
        first = True
        for n in reversed(range(1,m-1)):
            s = sum([x**2 for x in range(n,m)])
            #print "\nrange(%d,%d) = %s" %(n,m,range(n,m)),
            if s < lim:
                yield s
                first = False
            elif first:
                return
            else:
                break

def is_palindrome(n):
    s = str(n)
    return s == s[::-1]

if __name__ == "__main__":
    start = time()
    main()
    #for n in consec_square_sums(1000):
    #    print "%d" %n,
    print "That took %f seconds" %(time() - start)