#!/usr/bin/python

from common import prime_generator

def nth_prime(n):
	i = 1
	for prime in prime_generator():
		i += 1
		if i > n: return prime

if __name__ == "__main__":
	print(nth_prime(10001))
