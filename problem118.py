#!/usr/bin/python

"""http://projecteuler.net/problem=118"""

from time import time
from itertools import permutations, combinations
from sys import stdout

def main():
	seen = set()
	digits = '123456789'
	for order in permutations(digits,9):
		stdout.write("%s\r" %(''.join(order)))
		stdout.flush()
		for group in prime_groups_gen(order):
			seen.add(group)
	print "\nThere are %d sets" % len(seen)

def prime_groups_gen(s):
	for combo in bin_list_gen(len(s)-1):
		ps = frozenset(weave(s,combo))
		if all_prime(ps):
			yield ps

def all_prime(s):
	for i in s:
		if not is_prime(i):
			return False
	return True

def bin_list_gen(l):
	b_list = [True] * l
	yield b_list
	while True in b_list:
		# set the first False value to True
		for i in range(l):
			if b_list[i]:
				b_list[i] = False
				break
			else:
				b_list[i] = True
		yield b_list

def weave(digits,combo):
	num = int(digits[0])
	out = []
	for d,c in zip(digits[1:],combo):
		if c:
			num = num * 10 + int(d)
		else:
			out.append(num)
			num = int(d)
	out.append(num)
	return out

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

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
