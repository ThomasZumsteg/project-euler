#!/usr/bin/python
"""http://projecteuler.net/problem=108"""

from time import time
from sys import stdout
from itertools import count, combinations

def main():
	big_num = 1000
	biggest = 0
	for n in count(1):
		solutions = num_solutions(n)
		stdout.write("%3d: %d\r" %(n,solutions))
		stdout.flush()
		if solutions > big_num:
			print "%3d: %d" %(n, solutions)

def num_solutions(n):
	num = 0
	for x in range(n+1, 2*n+1):
		if x == n: continue
		if (n * x) % (x - n) == 0:
			num += 1
			print "1/%d = 1/%d + 1/%d" %(n,x,(n*x)/(x-n))
	return num

def factors(num):
	facts = []
	for f in range(2,num):
		while num % f == 0:
			num /= f
			facts.append(f)
	return str(sorted(facts))

if __name__ == "__main__":
	start = time()
#	main()
	S = 'GOOGLE'
	seen = set()
	i = 0
	for n in range(1,len(S)+1):
		for g in combinations(S,n):
			if g in seen: continue
			i += 1
			seen.add(g)
			g = sorted([str(x) for x in g])
			print "%d: %s" %(i, ''.join(g))
	print "That took %f seconds" %(time() - start)
