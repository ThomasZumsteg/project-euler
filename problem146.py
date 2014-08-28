#!/usr/bin/python
"""Finds positive integers n below 150 million for which n**2 + [1,3,7,9,13,27] are consecutive primes"""

from time import time
from common import is_prime, prime_sieve
from itertools import count
from sys import stdout

def main():
	s = 0
	start = time()
	primes = [2,3,5,7,11,13,17]
	diffs  = [1,3,7,9,13,27]
	for n in wheel_gen(diffs, primes):
		stdout.write("%d: %f\r" %(n, time() - start))
		stdout.flush()
		if n > 150000000: return s
		if matches(n**2):
			print n
			s += n

def matches(n):
	for diff in (1,3,7,9,13,27):
		if not is_prime(n + diff):
			return False
	return True

def wheel_gen(diffs, primes):
	length = reduce(lambda x,y: x*y, primes, 1)
	wheel = [True] * length
	for p in primes:
		spoke = small_wheel(p, diffs)
		wheel = [w & s for w,s in zip(wheel, spoke * (length/p))]
	wheel_nums = [i for i,v in enumerate(wheel, 1) if v]
	print "Wheel created"
	for i in count(0):
		for j in wheel_nums:
			yield i * length + j

def small_wheel(n,diffs):
	l = [True] * n
	for i in range(len(l)):
		m = i + 1
		o = ( (m % n)**2 ) % n
		for p in diffs:
			if (o + p) % n == 0:
				l[i] = False
				break
	return l

def old_main():
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

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %(main())
	print "That took %f seconds" %(time() - start)
