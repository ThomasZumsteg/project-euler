#!/usr/bin/python
"""http://projecteuler.net/problem=65"""

from time import time

def main():
	i = 100
	terms = e_seq(i)
	numer = 1
	denom = terms.pop()
	for t in reversed(terms):
		[numer, denom] = [ denom , numer + denom * t ]
	[denom, numer] = [numer, denom]
#	print "%d / %d" %(numer, denom)
	print "Sum digits of the numberator of the 100th convergents:"
	print sum([int(x) for x in list(str(numer))])

def e_seq(n):
	seq = [2]
	k = 2
	while len(seq) < n:
		seq.extend([1,k,1])
		k += 2
	return seq[:n]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)

