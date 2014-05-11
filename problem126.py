#!/usr/bin/python
"""http://projecteuler.net/problem=126"""

from time import time
from itertools import count
from sys import stdout

def main():
	big_num = 1000
	tree = {}
	lim = 50000
	for ways in ways_gen(lim):
		try: tree[ways] += 1
		except KeyError: tree[ways] = 1
	for k in sorted(tree.keys()):
		if tree[k] == big_num:
			print "%d: %d" %(k, tree[k])
			return
	print "\n" + str(max(tree.values()))

def ways_gen(lim):
	for l in count(1):
		if cube_layer(1,1,1,l) > lim: break
		for x in count(1):
			if cube_layer(x,1,1,l) > lim: break
			for y in range(1,x+1):
				if cube_layer(x,y,1,l) > lim: break
				for z in range(1,y+1):
					cl = cube_layer(x,y,z,l)
					stdout.write("(%3d, %3d, %3d, %2d): %6d    \r" %(x,y,z,l,cl))
					stdout.flush()
					if cl > lim: break
					yield cl

def cube_gen(lim):
	for x in range(1,lim):
		for y in range(1,x+1):
			for z in range(1,y+1):
				yield (x,y,z)

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
		f = cube_layer_gen_new(l,m,n)
		v = f.next()
		if v in d: d[v].append(f)
		else: d[v] = [f]
		#print "Created (%d,%d,%d)=%d" %(l,m,n,v)
	#return c + 1

def cube_layer(x,y,z,l):
	top  = (x * y) + (x * z) + (y * z)
	side = x + y + z + (l - 2)
	return 2 * top + 4 * (l - 1) * side

def cube_layer_gen_new(l,m,n):
	for i in count(1):
		top = (l * m) + (l * n) + (m * n)
		side = l + m + n + (i - 2)
		yield 2 * top + 4 * (i - 1) * side

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
	main()
	#box = (1,1,1)
	#for i,(a,b) in enumerate(test_gen(cube_layer_gen, cube_layer_gen_new, box)):
	#	print "%d: (%d, %d)" %(i,a,b)
	#	if i > 10: break
	print "That took %f seconds" %(time() - start)
