#!/usr/bin/python
from math import sqrt
from re import sub
from itertools import count

def is_prime(n):
	if n<2:
		return False
	if n in (2,3):
		return True
	if n % 2 == 0 or n % 3 == 0:
		return False
	max_divisor = int(n**0.5)+1
	divisor = 5
	while divisor <= max_divisor:
		if n % divisor == 0 or n % (divisor + 2) == 0:
			return False
		divisor += 6
	return True

def prime_factors(num):
	factors = []
	for prime in prime_generator():
		while num % prime == 0:
			factors += [prime]
			num /= prime
		if num == 1:
			return factors

def prime_generator(block=100000):
	primes = []
	for i in count(0):
		if len(primes) <= i:
			#print "Extended"
			primes.extend(prime_sieve(block, primes))
		yield primes[i]

def prime_sieve(block,primes=[]):
	"""Sieves a block of prime numbers"""
	if not primes:
		offset = 3
		extend = (block - offset) // 2 + 1
		nums = [ True ] * extend
	else: 
		# Problem extending, 100133 and 100135 are not prime
		offset = primes[-1] + 2
		nums = primes_extender(primes,block)
		#print nums
		extend = len(nums)
	for i in range(extend):
		if nums[i]:
#			print "Found %d, i:%d" % (2*i+offset,i)
			for j in range(3*i+offset,extend,2*i+offset):
#				print "%d is not prime (%d * %d),j:%d" %(2*j+offset,2*i+offset,(2*j+offset)/(2*i+offset),j)
				nums[j] = False
#	print nums
	if primes:
		return [2*i+offset for i in range(len(nums)) if nums[i]]
	else:
		return [2]+[2*i+offset for i in range(len(nums)) if nums[i]]

def primes_extender(primes,block):
	start = primes[-1] + 2
	length = block
	num = [ True ] * length
	if 2 == primes[0]: first = 1
	else: first = 0
	for p in primes[first:]:
		i_start = start % p
		if i_start > 0: i_start = p - i_start // 2
		for i in range(i_start, length, p):
			#print "prime: %d, number: %d" %( p, i*2+start)
			num[i] = False
	return num

def prime_generator_old():
	list_of_primes = [2,3,5,7,11,13]
	for prime in list_of_primes:
		yield prime
	new_prime = 15
	while True:
		prime = True
		root = int(sqrt(new_prime))
		for a_prime in list_of_primes:
			if root < a_prime:
				break
			if new_prime % a_prime == 0 :
				new_prime += 2
				prime = False
				break
		if prime:
			list_of_primes += [new_prime]
			yield new_prime
			new_prime += 2

def factors_of_num(number):
	factors = [1]
	for i in range(2,int(sqrt(number))+1):
		if number % i == 0:
			factors.append(i)
			if i != number/i:
				factors.append(number/i)
	return factors 

def counter(count=0):
	while True:
		yield count
		count += 1

def pandigital_len(string):
	for i in range(1,10):
		if string.count(str(i)) == 1:
			string = sub(str(i),'',string)
		else:
			break
	if len(string) == 0:
		return i-1
	else:
		return -1

def lexicographic_gen(o_list):
	done = False
	indexes = [0] * len(o_list)
	while not done:
#		print indexes
		temp = o_list
		string = ''
		for i, index in enumerate(indexes):
			string += temp[index]
			temp = temp[:index] + temp[index+1:]
		yield string
		indexes[-1] += 1
		(done, indexes) = reorder(indexes)

def reorder(indexes):
	i_lim = range(len(indexes))
	done = False
	swap_next = False
	for lim, i, val in zip(i_lim,reversed(i_lim),reversed(indexes)):
#		print "index:%d val:%d limit:%d" %(i,val,lim) 
		if swap_next:
			indexes[i] += 1
			val += 1
		swap_next = False
		if val > lim:
#			print "swapped"
			indexes[i] = 0
			swap_next = True 
	return (swap_next, indexes)

if __name__ == "__main__":
	primes = [2,3,5,7]
	print primes_extender(primes,30)
