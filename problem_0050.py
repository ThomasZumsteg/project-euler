#!/usr/bin/python
"""http://projecteuler.net/problem=50"""

from time import time

def main():
	block = 100000
	start = time()
	primes = prime_sieve(block)
	i = 0
	max_i = 0
	max_prime_set = 0
	while True:
		i += 1
		if i >= len(primes):
			primes = prime_sieve(block,primes)
		if primes[i] > 1000000: break
#		print "Testing %d" %(primes[i])
		prime_set = is_prime_sum(primes[i],primes, max_prime_set)
		if prime_set and max_prime_set < len(prime_set):
			max_prime_set = len(prime_set)
			max_i = i
#			print "%d is the sum of %d primes" %(primes[i], max_prime_set)
	print "%d is the sum of %d primes" %(primes[max_i], max_prime_set)
	print "That took %f seconds" %(time()-start)

def is_prime_sum(num,primes,sum_min):
	i = 0
	while True:
		prime_sum = primes[i:i+sum_min]
		if sum(prime_sum) > num: return False
		j = i + sum_min 
		while True:
			prime_sum.append(primes[j])
			if sum(prime_sum) > num: break
			elif sum(prime_sum) == num: return prime_sum
			j += 1
		i += 1

def prime_sieve(extend,primes=[]):
#	print "Called"
	nums = [ True ] * (extend)
	if not primes: offset = 2
	else: 
		offset = primes[-1]+1
		for p in primes:
			start = ( offset - 1 ) // p + 1
			end   = ( offset + extend - 1 ) // p + 1
			for n in range(start, end):
#				print "%d is not prime (%d * %d)" %(n*p, n,p)
				nums[n*p-offset] = False
#				print "Marked %d False" % (n*p-offset)
	for i in range(extend):
		if nums[i]:
#			print "Found %d" % (i+offset)
			primes.append(i+offset)
			for n in range(2,1+extend//(i+offset)):
#				print "%d is not prime it %d * %d" %((i+offset)*n, n, i+offset)
				nums[(i+offset)*n-offset] = False
#	print nums
	return primes

def prime_generator(block=100000):
	primes = []
	i = 0
	while True:
		if len(primes) <= i:
			primes = prime_sieve(block, primes)
		yield primes[i]
		i += 1

if __name__ == "__main__":
	main()
#	primes = prime_sieve(10000)
#	print is_prime_sum(41,primes,2)
