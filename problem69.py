#!/usr/bin/python
"""http://projecteuler.net/problem=69"""

from time import time

def main():
	num = 10**7
	max_r_primes = None
	max_r = 0
	set_list = sieve(num)
	print "Sieve Done"
	for n, i_set in enumerate(set_list[1:]):
		phi = n+2
		for i in i_set:
			phi *= (1-1.0/i)
#		print "n:%d, phi:%d, fact: %s" %(n+2,phi,str(i_set))
		if (n+2)/float(phi) > max_r_primes or not max_r_primes:
			max_r = n+2
			max_r_primes = (n+2)/float(phi)
	print "%d is %0.3f time larger then the number of relatively prime numbers it has" %(max_r, max_r_primes)

def sieve(num):
	set_list = [set() for x in range(num)]
	for i in range(1,len(set_list)):
		print i
		if len(set_list[i]) == 0:
			for j in range(i,num,i+1):
				set_list[j].add(i+1)
	return set_list

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
