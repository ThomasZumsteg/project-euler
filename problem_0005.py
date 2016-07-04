#!/usr/bin/python
"""2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"""

from common import prime_factors

def smallest_product_of_primes_under_n(n):
	common_factors = []
	for i in range(2,n+1):
		i_prime_factors = prime_factors(i)
		common_factors = contains_and_expand(i_prime_factors, common_factors)
	
	product = 1
	for i in common_factors:
		product *= i
	return product

def contains_and_expand(list_a, list_b):
	for i in list_a:
		if i in list_b:
			list_b.remove(i)
	return list_a + list_b

if __name__ == "__main__":
	print smallest_product_of_primes_under_n(20)
