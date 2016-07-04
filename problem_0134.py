#!/usr/bin/python
"""http://projecteuler.net/problem=134"""

from time import time
from sys import stdout
from common import prime_generator, is_prime
from itertools import count

def main():
	#big_num = 10000
	big_num = 1000000
	s_sum = 0
	pg = prime_generator(big_num + 1000)
	large = pg.next()
	large = pg.next()
	large = pg.next()
	#print large
	for p in pg:
		(large, small) = (p, large)
		if small > big_num: break
		stdout.write("%d: %d\r" %(large, small))
		stdout.flush()
		s_sum += get_s(large, small) 
		#if get_s(large,small) != slow_get_s(large, small):
		#	print "Problem: (%d, %d)" %(large,small)
	print "Sum of all s is %d" %(s_sum)

def prime_pairs(lim, block = 1000100):
	pg = prime_generator(block)
	large = pg.next()
	for p in pg:
		(large, small) = (p, large)
		if small >= lim: return
		if large - small == 2:
			yield (large, small)
			
def get_s(p2, p1):
	ord = get_ord(p1)
	(_, (b, a)) = diophantine(p2, -ord, p1)
	#print "(%d, %d)" %( b, a)
	return (b * -int(a//b) + a) * ord + p1

def slow_get_s(p2, p1):
	ord = get_ord(p1)
	rem = p1
	for n in count(0):
		num = ord * n + p1
		if num % p2 == 0:
			return num

def diophantine(x,y,z):
	# solve alfa * x + beta * y = z for integers alfa and beta
	# beta = f(a_m, a_b) = b_m * i + b_b
	# alfa = f(a_m, b_b) = a_m * i + a_b
	q = []
	(d, r) = (x, y)
	for i in count(0):
		q.append(d//r)
		#print "%d = %d * %d + %d" %(d, q[-1], r, d % r)
		(r, d) = (d % r, r)
		if r == 0: break
	# d is now gcf(x,y)
	(big, small) = (0,1)
	for n in reversed(q[:-1]):
		(big, small) = (small, big - small * n)
		#print "(%d): %d: %d" %(n, big, small)
	#print "big: %d. small: %d" %(z // d * big, z // d * small)
	#print "%d = (%d + %d * n) * %d + (%d + %d * n) * %d" %(z, z // d * big, y//d, x, z // d * small, -x // d, y)
	#        (   a_m,          a_b), (   b_m,            b_b)
	return ((y // d, z // d * big), (-x // d, z // d * small))

def get_ord(n):
	for i in count(1):
		n = n // 10
		if n == 0: return 10 ** i

if __name__ == "__main__":
	start = time()
	main()
	#print get_s(23,19)
	print "That took %f seconds" %(time() - start)
