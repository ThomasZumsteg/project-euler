#!/usr/bin/python
"""http://projecteuler.net/problem=91"""

from time import time
from itertools import product, combinations

def main():
	sets = set()
	size = 51
	for p in product(range(size),repeat=2):
		for q in product(range(size),repeat=2):
#			print "%s: %s" %(str(p),str(q))
			if is_valid(p,q):
#				print p,q
				pq = frozenset((p,q))
				sets.add(pq)
	print "Found %d sets" %(len(sets))

def is_valid(p,q):
	# Test orthogonality 
	a = p[0]*q[0] + p[1]*q[1]
	b = q[0]*(p[0]-q[0])+q[1]*(p[1]-q[1])
	c = p[0]*(p[0]-q[0])+p[1]*(p[1]-q[1])
	zeros = [a,b,c].count(0)
#	print "a: %d, b: %d, c: %d, zeros: %d" %(a,b,c,zeros)
	if zeros == 1:
		return True
	else:
		return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
