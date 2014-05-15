#!/usr/bin/python
"""http://projecteuler.net/problem=128"""

from time import time
from itertools import count

def main():
	seq = 0
	lim = 3
	for n in count(1):
		for m in gen_neighbors(n):
			p = 0
			if diff(m,n) in primes: p += 1
			if p > lim: break
		if p == lim: seq += 1

def gen_neighbours(n):
	if (n - 2) % 6

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
