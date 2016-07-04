#!/usr/bin/python
"""Euler discovered the remarkable quadratic formula:

	n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

The incredible formula  n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, -79 and 1601, is -126479.

	Considering quadratics of the form:

	n^2 + an + b, where |a| < 1000 and |b| < 1000

	where |n| is the modulus/absolute value of n
	e.g. |11| = 11 and |-4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0."""

from common import prime_generator, counter

def main():
	n_prime = prime_generator()
	primes = [n_prime.next()]
	max_conseq = 0
	for a in range(-999,1000):
		for b in range(-999,1000):
			consec = consec_quad_primes(a,b,primes,n_prime)
			if max_conseq < consec:
				max_conseq = consec
				coeff = (a,b)
		print a
	print "%d: from %s" %(max_conseq, str(coeff))

def consec_quad_primes(a,b,primes,prime_gen):
	for n in counter(1):
		num = n**2 + a * n + b
		while primes[-1] < num:
			primes.append(prime_gen.next())
		if num not in primes:
			return n

if __name__ == "__main__":
	main()
