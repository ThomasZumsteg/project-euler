#!/usr/bin/python
"""http://projecteuler.net/problem=46"""

from common import prime_generator

def main():
	n = 1
	p_gen = prime_generator()
	prime_list = [p_gen.next()]
	while True:
		n += 2
		while n > prime_list[-1]:
			prime_list.append(p_gen.next())
		if n in prime_list:
			continue
		if not fits_conjecture(n,prime_list):
			print n
			break

def fits_conjecture(num, primes):
	for n in range(1,num):
		i = 0
		while True:
			gold_sum = primes[i] + 2 * n**2
			if gold_sum > num: break
			elif gold_sum == num:
#				print "%d = %d + 2 * %d^2" %(num, primes[i], n)
				return True
			i += 1
	return False

if __name__ == "__main__":
	main()
