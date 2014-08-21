#!/usr/bin/python
"""Counts the number reversible numbers below one-billion. A reversible number is one that for which sum(n + reverse(n)) has all odd digits
  Details: http://projecteuler.net/problem=145"""

from time import time
from itertools import count
from sys import stdout

def main():
	return brute(10**9)

def brute(lim):
	"""Cycles through all numbers less then the lim checking for reversiable
	Slow (~40mim/10**9) but it works"""
	i = 0
	# range(1,lim) overruns memory
	for n in count(1):
		if n >= lim: return i
		if reversible(n):
			i += 1
			stdout.write("%7d: %10d\r" %(i,n))
			stdout.flush()

def reversible(n):
	"""Returns True if n + reverse(n) has all odd digits, and doesn't end in 0"""
	if n % 10 == 0: return False
	for d in str(int(str(n)[::-1]) + n):
		if int(d) % 2 == 0: return False
	return True

if __name__ == "__main__":
	start = time()
	print "Answer: %d     " %(main())
	print "That took %f seconds" %(time() - start)