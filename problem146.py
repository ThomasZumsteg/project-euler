#!/usr/bin/python
"""Finds positive integers n below 150 million for which n**2 + [1,3,7,9,13,27] are consecutive primes"""

from time import time
from common import is_prime, prime_sieve, prime_generator
from itertools import count
from sys import stdout

def main():
	s = 0
	lim = 150000000
	start = time()
	diffs  = (1,3,7,9,13,27)
	for n in wheel_sieve(diffs, lim, 1000):
		stdout.write("%9d: %7.1f\r" %(n, time() - start))
		stdout.flush()
		if matches(n**2, diffs):
			s += n
			print "%9d: %7.1f: %10d" %(n, time() - start, s)
	return s

def matches(n,diffs):
	for d in range(1,29,2):
		if is_prime(n+d):
			if d not in diffs:
				return False
		else:
			if d in diffs:
				return False
	return True

prime_list = []

def primes_less_then(n):
	global prime_list
	block = 100000
	for i in count(1):
		if i >= len(prime_list):
			prime_list.extend(prime_sieve(block, prime_list))
		if prime_list[i] > n: break
		yield prime_list[i]

def wheel_sieve(diffs, lim, stop):
	wheel = {}
	for p in prime_generator(block=(stop+100)):
		stdout.write("%9d\r" %(p))
		stdout.flush()
		if p > stop: break
		if p == 2 or p == 5: continue
		spoke = wheel_worker(p, diffs)
#		print "%2d: %s" %(p, spoke)
		wheel[p] = spoke
	primes = sorted(wheel.keys())
	for i in range(lim//10):
		for p in primes:
			flag = True
			if not wheel[p][i % p]:
				if i == 1: print "%d: %d: %s" %(p,i%p,wheel[p])
				flag = False
				break
		if flag: yield i*10

def wheel_gen(diffs, primes):
	l = reduce(lambda x,y: x*y, primes)
	wheel = [True] * l
	for p in primes:
		print p
		spoke = wheel_worker(p, diffs)
		for i in range(l):
			if wheel[i]:
				wheel[i] &= spoke[i % p]
	wheel_nums = [i*10 for i,v in enumerate(wheel) if v]
	print "Wheel created"
	for i in count(0,10):
		for j in wheel_nums:
			yield i * l + j

def wheel_worker(n, diffs):
	spoke = [True] * n
	for i in range(n):
		mod = ((i*10) % n)**2
		for d in diffs:
			if (mod + d) % n == 0 and (mod + d) != n:
				spoke[i] = False
				break
	return spoke

def small_wheel(n,diffs):
	l = [True] * n
	for i in range(n):
		m = 10 * i
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
	s = 0
	for n in range(10,lim,10):
		stdout.write("%d\r" % n)
		stdout.flush()
		for d in (1,3,7,9,13,27):
			flag = True
			if not is_prime(n**2 + d):
				flag = False
				break
		if flag:
			s += n
			print "%3d: %3d" %(n, s)
	return s

def test(n):
	for m in range(0,2*10*n, 10):
		print "%3d" % m,
		flag = True
		for d in (1,3,7,9,13,27):
			if ((m % n)**2 + d) % n == 0:
				flag = False
				print "%5d^2 + %d = %d * %d" % (m, d, n, (m**2+d)//n)
				break
		if flag:
			print " True"

if __name__ == "__main__":
	start = time()
	print "Answer: %s" %(main())
	print "That took %f seconds" %(time() - start)
