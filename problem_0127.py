#!/usr/bin/python
"""http://projecteuler.net/problem=127"""

from time import time
from sys import stdout
from itertools import combinations

def main():
	lim = 1000
	fact = fact_sieve(lim)
	c_sum = 0
	for c in range(3,lim):
		if not fact[c] or rad(fact[c]) == c: continue
		stdout.write("%d\r" %(c))
		stdout.flush()
		C = fact[c]
		for b in range(c//2+1,c):
			B = fact[b]
			if C & B: continue
			a = c - b
			A = fact[a]
			if C & A or B & A: continue
			if rad(A | B | C) < c:
				print "(%3d, %3d, %3d)" %(c,b,a),
				print "([%6s], [%6s], [%6s])" % tuple([','.join(x) for x in map(lambda s: [str(e) for e in s],(C,B,A))])
#				print "(%s, %s, %s)" %(gcd(A,B),gcd(A,C),gcd(B,C)),
#				print "%d" %(rad(A|B|C))
				c_sum += c
	print "Sum is %d" %(c_sum)

def hit(a,b,c,lim):
	if gcd(a,b) or gcd(a,c) or gcd(b,c):
		return False
	return rad(a | b | c) < lim

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

def rad(s):
	if s:
		return reduce(lambda x,y: x * y, s)
	else:
		return 1

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)

