#!/usr/bin/python
"""Find the sum of all such n's less then one trillion

Given n,d,q and r such that:
	1) q and r are the quotient and remainer of n divided by d
	2) n, d and r are consecutive positive integers in a geometric sequence, but not necessarialy in that order.
	3) n is a perfect square

Details: http://projecteuler.net/problem=141"""

from time import time
from itertools import count

def main():
	brute(100)

def brute(lim):
	for b in count(1):
		for f in square_factor(b):
			for n in count(1):
				c = n / float(f)
				n = c * (c**3 * b + 1)
				if n > lim: break
				q = b
				d = 
				r = b
				print "%d = %d * %d + %d: %s" %(n,q,d,r, sorted([q,d,r]))

def square_factor(b):
	f = []
	for n in range(1,int(b**0.5)):
		if b % (n**2) == 0:
			f.append(n)
	return f

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %( main() )
	print "That took %f seconds" %(time() - start)
