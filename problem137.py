#!/usr/bin/python
"""Solved http://projecteuler.net/problem=137"""

from time import time
from itertools import count
from sys import stdout

def main():
	nuggets = 1
	for n in count(1):
		stdout.write("%2d: %9d\r" %(nuggets, n))
		stdout.flush()
		if is_square(5*n**2 + 2*n + 1):
			print "%2d: %9d" %(nuggets, n)
			if nuggets >= 15: return
			nuggets += 1


def is_square(N):
	return N == (int(N**0.5))**2

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
