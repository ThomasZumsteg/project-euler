#!/usr/bin/python
"""Finds all Torricelli triangels with integer sides and interior lines p,q, and r with interior lines summing to 120,000 or less
  (Much) more detail: http://projecteuler.net/problem=143"""

from time import time
from sys import stdout
from itertools import count

def main():
	lim = 120000
	s = set()
	f = lambda x,y: (x**2 + x * y + y**2)**0.5
	for (p,q,r) in sol_gen(lim):
		print "(%5d, %5d, %5d) (%5d, %5d, %5d): %7d" %(p,q,r,f(p,q),f(p,r),f(q,r),sum((p,q,r)))
		s.add(sum((p,q,r)))
	return sum(s)

def sol_gen(lim):
	solutions = {}
	for p in range(1, lim-1):
		p_set = set()
		for q in range(1,min(p,lim-p)):
			A = p**2 + p * q + q**2
			if is_square(A):
				#print "(%d, %d)" %(p,q)
				if q in solutions:
					for r in p_set:
						if r in solutions[q] and p+q+r < lim:
							yield (p,q,r)
				p_set.add(q)
		if p_set: solutions[p] = p_set

def is_square(n):
	"""Does what it says on the tin"""
	m = int(n**0.5)
	return m**2 == n

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %( main())
	print "That took %f seconds" %(time() - start)