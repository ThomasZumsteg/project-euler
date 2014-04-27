#!/usr/bin/python
"""http://projecteuler.net/problem=58"""

from common import counter,prime_sieve
from time import time

def main():
	count = 1
	n_primes = 0
	primes = prime_sieve(100000)
	for s in spiral_gen():
		if s**0.5 > primes[-1]:
#			print "extending %d: %d" %(primes[-1], s)
			primes.extend(prime_sieve(100000, primes))
		if is_prime(s,primes):
			n_primes += 1
#			print s
		count += 1
#		side_len = 2 * ((count + 2)// 4) + 1
#		print "%d: %d" %(side_len, s)
#		if count > 30: break
		if float(n_primes)/count < 0.1:
			side_len = 2 * ((count + 2)// 4) + 1
			print "%f: %d" %( 100 * float(n_primes)/count, side_len)
#			print "%d/%d = %f" %(n_primes, count,  100 * float(n_primes)/count)
			return None

def is_prime(num, primes):
	for prime in primes:
		if prime > num**0.5: return True
		if num % prime == 0: return False

def spiral_gen():
	count = 1
	for i in counter():
		for j in range(4):
			count += (i+1)*2
			yield count
#			print "Adding %d" % count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" % (time() - start)
#	primes = prime_sieve(1000000)
#	count = 0
#	for prime in primes:
#		if not is_prime(prime, primes):
#			print "Problem %d" %prime
#		count += 1
#		if count > 10000: break
