#!/usr/bin/python
"""http://projecteuler.net/problem=47"""

from common import prime_generator,counter

def main():
	nums = []
	p_gen = prime_generator()
	primes = [p_gen.next()]
	for i in counter(1):
		factors = 0
		while i/30 >= primes[-1]:
			primes.append(p_gen.next())
		for n in primes:
			temp = i
			once = True
			while temp % n == 0:
				if once:
					factors += 1
					once = False
				temp /= n
			if i/30 < n or 4 < factors:
				break
#		if i % 1000 == 0:
#			print str(i) + " has " + str(factors) + " factors "
		if factors == 4:
			nums.append(i)
		else:
			nums = []
		if len(nums) == 4:
			break
	print nums

if __name__ == "__main__":
	main()
