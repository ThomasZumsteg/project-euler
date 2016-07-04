#!/usr/bin/python
"""http://projecteuler.net/problem=106"""

from time import time
from sys import stdout
from itertools import combinations

def main():
	count = 0
	groups = set()
	A = 'ABCDEFGHIJKL'
	# Genearte all same sized subsets of size 2 or greater
	# size 1 taken care of by problem statement
	for n,B,CD in split(A):
		for C in combinations(CD,n):
			pair = sorted((''.join(B),''.join(C)))
			groups.add(tuple(pair))
	# Test unique same sized groups
	# count those that have at least one bigger elementpair and one smaller element pair
	# eg (AD,CB) not (AC,BD)
	for i,(B,C) in enumerate(groups):
		signs = []
		for b,c in zip(B,C):
			signs.append(b<c)
		if True in signs and False in signs:
			count += 1
#			print "Test: %s, %s" %(B,C)
	print "%d values need testing" %( count )

def split(A):
	for n in range(2,len(A)//2 + 1):
		for B in combinations(A,n):
			C = tuple([x for x in A if x not in B])
			yield n,B,C

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
