#!/usr/bin/python

from common import prime_generator

def main():
	limit = 2e6
	prime_sum = 0
	for prime in prime_generator():
		if prime > limit: break
		prime_sum += prime
	print prime_sum

if __name__ == "__main__":
	main()
