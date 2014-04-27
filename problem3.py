#!/usr/bin/python
"""The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?"""

from math import sqrt

def prime_factors_slow(num):
	"""takes to long recursive"""
	for i in range(2,num):
		if num % i == 0:
			fact1 = prime_factors(i)
			fact2 = prime_factors(num/i)
			return fact1 + fact2
	return [ num ]

def prime_factors(num):
	factors = []
	for prime in prime_generator():
		while num % prime == 0:
			factors += [prime]
			num /= prime
		if num == 1:
			return factors

def prime_generator():
	list_of_primes = [2,3,5,7,11,13]
	for prime in list_of_primes:
		yield prime
	new_prime = 15
	while True:
		prime = True
		root = int(sqrt(new_prime))
		for a_prime in list_of_primes:
			if root < a_prime:
				break
			if new_prime % a_prime == 0 :
				new_prime += 2
				prime = False
				break
		if prime:
			yield new_prime
			new_prime += 2

if __name__ == "__main__":
	print prime_factors(600851475143)
