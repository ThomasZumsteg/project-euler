#!/usr/bin/python
"""http://projecteuler.net/problem=87"""

from time import time
from common import prime_sieve
from sys import stdout

def main():
	# for all prime**4ths
	# for all primes**3s
	# if there's a prime**2 add one
	lim = 50000000
	num_list = [False] * lim
	prime_list = prime_sieve( int(lim**0.5) )
	for a in prime_list:
		if a**4 > lim: 
			print "Done at %d" % a
			break
		for b in prime_list:
			if a**4 + b**3 > lim: break
			for c in prime_list:
				stdout.write("%2d, %5d, %10d, %d\r" %(a,b,c,a**4+b**3+c**2))
				stdout.flush()
				num = a**4 + b**3 + c**2 
				if num > lim: break
#				print "%d^4+%d^3+%d^2=%d" %(a,b,c,a**4+b**3+c**2)
				num_list[num] = True
	count = 0
	for n in num_list:
		if n:
			count += 1
	print "There are %d" % count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
