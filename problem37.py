#!/usr/bin/python
"""http://projecteuler.net/problem=37"""

from common import prime_generator

def main():
	prime_list = []
	trunc_prime_sum = 0
	trunc_prime_count = 0
	for prime in prime_generator():
		prime_list.append(prime)
		if prime < 10:
			continue
#		print "Checking prime %d" % prime
		trunc_prime = True
		for i in range(1,len(str(prime))):
			left  = int(str(prime)[:i])
			right = int(str(prime)[i:])
#			print "Testing %d and %d" %(left, right)
			if left not in prime_list or right not in prime_list:
#				print "Nope!"
				trunc_prime = False
				break
		if trunc_prime:
			trunc_prime_count += 1
			print prime
			trunc_prime_sum += prime
		if trunc_prime_count >= 11:
			break
	print trunc_prime_sum

if __name__ == "__main__":
    main()
