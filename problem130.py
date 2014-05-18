#!/usr/bin/python
"""http://projecteuler.net/problem=130"""

from time import time
from common import is_prime
from itertools import count

def main():
	hits = 0
	total = 0
	lim = 25
	for n in count(7,2):
		if n % 5 == 0 or is_prime(n): continue
		a = A(n)
		refresh("%d: %d", (n,a))
		if (n - 1) % a == 0:
			print "%d: %d" %(n,a)
			total += n
			hits += 1
			if hits >= lim: break
	print "Sum of first %d values is %d" %(lim, total)

def A(n):
	rep = 1
	for i in count(1):
		rep %= n 
		if rep == 0:
			return i
		rep = rep * 10 +1 

def refresh(form, args):
	from sys import stdout
	stdout.write(form % args + "\r")
	stdout.flush()

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)