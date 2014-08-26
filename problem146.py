#!/usr/bin/python
"""Finds positive integers n below 150 million for which n**2 + [1,3,7,9,13,27] are consecutive primes"""

from time import time
from common import prime_generator, is_prime, is_square
from itertools import count
from sys import stdout

def main():
	nums = range(6)
	s = 10
	for n in brute(150000000):
		stdout.write("%d\r" %(n))
		stdout.flush()
		prime_flag = True
		for i,d in enumerate((1,3,7,9,13,27)):
			nums[i] = n**2 + d
			if not is_prime(nums[i]):
				prime_flag = False
				break
		if prime_flag:
			s += n
			print "%3d: [%6d, %6d, %6d, %6d, %6d, %6d]" % tuple([n] + nums)
	return s

def brute(lim):
	# 10, 315410, 927070
	for n in range(10,lim,10):
		yield n

def matches(n):
	for diff in (1,3,7,9,13,27):
		if not is_prime(n + diff):
			return False
	return True

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %(main())
	print "That took %f seconds" %(time() - start)