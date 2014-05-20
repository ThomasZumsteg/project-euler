#!/usr/bin/python
"""http://projecteuler.net/problem=134"""

from time import time
from sys import stdout
from common import prime_generator
from itertools import count

def main():
	big_num = 1000000
	p_big = 1
	s_sum = 0
	for p in prime_generator(block = big_num + 100):
		(p_big, p_small) = (p, p_big)
		if p_big - p_small != 2: continue
		if p_big == 5: continue
		if p_small > big_num: break
		stdout.write("%d: %d\r" %(p_big, p_small))
		stdout.flush()
		s_sum += find_s(p_small, p_big)
	print "The s sum is %d" %(s_sum)

def find_s(p1, p2):
	rem = p1
	ord = get_ord(p1)
	for n in count(1):
		rem = (ord + rem) % p2
		#print rem
		if rem == 0: return n

def get_ord(n):
	for i in count(1):
		n = n // 10
		if n == 0: return 10 ** i

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)