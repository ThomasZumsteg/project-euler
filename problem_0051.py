#!/usr/bin/python
"""http://projecteuler.net/problem=51"""

from re import sub
from time import time

def main():
	pattern_dict = {}
	for prime in prime_generator():
#		if prime > 100000: break
		for pattern in pattern_generator(str(prime)):
			if pattern in pattern_dict:
				pattern_dict[pattern].append(prime)
			else:
				pattern_dict[pattern] = [prime]
			if len(pattern_dict[pattern]) >= 8:
				print pattern_dict[pattern][0]
				return None
#	return pattern_dict

def pattern_generator(string):
	unique_digits = list(set(string))
	for digit in unique_digits:
		temp =  sub(digit,'.',string)
		yield temp 
		if temp.count('.') > 1:
			for pattern in sub_pattern(temp,digit):
				yield pattern

def sub_pattern(string,c):
	string = list(string)
	while True:
		for i in range(len(string)):
			if string[i] == c: string[i] = '.'
			elif string[i] == '.':
				string[i] = c
				break
		if '.' in string: yield ''.join(string)
		else: break
				
def prime_generator(block=100000):
	primes = []
	i = 0
	while True:
		if len(primes) <= i:
			primes = prime_sieve(block, primes)
		yield primes[i]
		i += 1

def prime_sieve(extend,primes=[]):
#	print "Called"
	nums = [ True ] * (extend)
	if not primes: offset = 2
	else: 
		offset = primes[-1]+1
		for p in primes:
			start = ( offset - 1 ) // p + 1
			end   = ( offset + extend - 1 ) // p + 1
			for n in range(start, end):
#				print "%d is not prime (%d * %d)" %(n*p, n,p)
				nums[n*p-offset] = False
#				print "Marked %d False" % (n*p-offset)
	for i in range(extend):
		if nums[i]:
#			print "Found %d" % (i+offset)
			primes.append(i+offset)
			for n in range(2,1+extend//(i+offset)):
#				print "%d is not prime it %d * %d" %((i+offset)*n, n, i+offset)
				nums[(i+offset)*n-offset] = False
#	print nums
	return primes

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time()-start)
#	for pattern in pattern_generator('56333'):
#		print pattern
