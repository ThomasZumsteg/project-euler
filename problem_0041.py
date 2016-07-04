#!/usr/bin/python
"""http://projecteuler.net/problem=41"""

from common import pandigital_len, prime_generator

def main():
	max_pan_len = (0,0)
	for prime in prime_generator():
		if prime > 987654321: break
		pan_len = pandigital_len(str(prime))
		if pan_len > max_pan_len[0]:
			max_pan_len = (pan_len, prime)
			print max_pan_len
	print max_pan_len

if __name__ == "__main__":
    print main()
