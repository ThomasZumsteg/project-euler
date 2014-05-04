#!/usr/bin/python
"""http://projecteuler.net/problem=126"""

from time import time
from itertools import count
from sys import stdout

def main():
	big_num = 1000
	tree = {}
	for n in range(1,40):
		ways = shift_counters(n,tree)
		if ways == big_num:
			print "%d is the least value" %(n)
			return
		if n%2==0: print "%d: %d" %(n, ways)
		make_counters(n,tree)

def shift_counters(n,d):
	if n not in d:
		#print "Skipped %d" %(n)
		return 0
	for c,f in enumerate(d[n]):
		v = f.next()
		if v in d:
			d[v].append(f)
		else:
			d[v] = [f]
		#print "Moved to %d" %(v)
	return c + 1

def make_counters(n,d):
	for c,(l,m,n) in enumerate(block_gen(n)):
		f = cube_layer_gen(l,m,n)
		v = f.next()
		if v in d: d[v].append(f)
		else: d[v] = [f]
		#print "Created (%d,%d,%d)=%d" %(l,m,n,v)
	#return c + 1

def cube_layer_gen_new(l,m,n):
	for i in count(1):
		tops = m * n
		side = (i + l - 1) * (m + n)
		otter= 2 * ((l - 1) * (i - 1) + sum(range(1,i)))
		yield 2 * (tops + side + otter)

def cube_layer_gen(l,m,n):
	#yield l*m*n
	center = 2 * (m + n)
	ends = [m*n]
	while True:
		#print "%d: %s" %(center, str(ends))
		yield l * center + 2 * sum(ends)
		ends.append(center)
		center += 4

def block_gen(n):
	# finds ways of making x,y,z such that x*y*z = n
	# given x >= y >= z
	yield (n,1,1)
	for a in range(2,n//2+1):
		if n % a != 0: continue
		for b in range(2,a+1):
			if n % (a * b) != 0: continue
			c = n // (a * b)
			if c <= b: yield (a,b,c)

def test_gen(a,b,args):
	f = a(*args)
	g = b(*args)
	while True:
		yield (f.next(), g.next())

if __name__ == "__main__":
	start = time()
#	main()
	box = (3,3,3)
	for i,(v_a,v_b) in enumerate(test_gen(cube_layer_gen,cube_layer_gen_new,box)):
		print "%d: (%d, %d)" %(i,v_a,v_b)
		if i > 4: break
	print "That took %f seconds" %(time() - start)
