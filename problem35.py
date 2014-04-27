#!/usr/bin/python
"""http://projecteuler.net/problem=35"""

from common import counter, prime_generator

def main():
	prime_gen  = prime_generator()
	prime_list = [prime_gen.next()]
	circular_count = 0
#	print "Starting"
	for i in counter():
		while i >= len(prime_list):
#			print "Extending primes array"
			new_prime = prime_gen.next()
			prime_list.append(new_prime)
		circular = True
		if prime_list[i] >= 1000000: break
#		print "Testing " + str(prime_list[i])
		for circ in circ_gen(prime_list[i]):
			while circ > prime_list[-1]:
				prime_list.append(prime_gen.next())
			if circ not in prime_list:
				circular = False
				break
		if circular: 
			print "This prime is circular " + str(prime_list[i])
			circular_count += 1
	print "There are %d circular primes" %circular_count

def circ_gen(number):
	str_num = str(number)
	for i in range(1,len(str_num)):
		yield int(str_num[i:] + str_num[:i])

if __name__ =="__main__":
	main()
