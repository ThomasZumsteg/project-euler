#!/usr/bin/python
"""http://projecteuler.net/problem=127"""

from time import time
from sys import stdout
from itertools import combinations

def main():
	lim = 120000
	fact = fact_sieve(lim)
	c_sum = 0
	for c in range(3,lim):
		stdout.write("%d\r" %(c))
		stdout.flush()
		for b in range(c//2+1,c):
			a = c - b
			(A,B,C) = (fact[a],fact[b],fact[c])
			if hit(A,B,C,c):
				#print "(%d, %d, %d)" %(c,b,a),
				#print "(%s, %s, %s)" %(gcd(A,B),gcd(A,C),gcd(B,C)),
				#print "%d" %(rad(A,B,C))
				c_sum += c
	print c_sum

def hit(a,b,c,lim):
	if gcd(a,b) or gcd(a,c) or gcd(b,c):
		return False
	return rad(a,b,c) < lim

def fact_sieve(lim):
	fact = [set([]) for _ in range(lim)]
	for i in range(2,lim):
		if not fact[i]:
			for j in range(i,lim,i):
				fact[j].add(i)
	return fact

def gcd(a,b):
	try: return max(a & b)
	except ValueError: return None

def rad(a,b,c):
	abc = a | b | c
	if abc:
		return reduce(lambda x,y: x * y, abc)
	else:
		return 1

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)

