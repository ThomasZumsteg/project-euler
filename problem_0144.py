#!/usr/bin/python
"""Find the number of internal reflections of a laser beam in cell with boundary 4*x^2 + y^2 = 10^2 and a gap at the top between -0.01 <= x <= +0.01. Start from (0.0, 10.1) and first impact at (1.4, -9.6)
  Details: http://projecteuler.net/problem=144"""

from time import time

def main():
	for i, p in enumerate(reflect_gen([0.0, 10.1], [1.4, -9.6]), 1):
		#print "%3d: (% 5.3f  % 5.3f)" % tuple([i] + p)
		if abs(p[0]) <= 0.01 and p[1] > 0: break
	# Last point is the escape and does not count
	return i-1

def reflect_gen(p,q):
	"""Generates a series of reflections for an intial point p and first impact q"""
	while True:
		yield q
		# n is a unit vector normal to the ellipse at point q
		n = unit([ci*qi for (ci,qi) in zip((-4,-1),q)])
		# d is a unit vector from p to q, e.g. the laser beams path
		d = unit([pi - qi for (pi,qi) in zip(p,q)])
		# r is a unit vector in the direction of the reflected laser beam
		r = reflect(d, n)
		(p,q) = (q, intersect(r, q))

def intersect(v, p):
	"""The intersection of vector v and cell 4 * x^2 + y^2 = 100 starting from p"""
	# Solve for t: 100 = 4 * (px + t * vx)^2 + (py + t * vy)^2
	# Note: 4 * px^2 + py^2 - 100 = 0
	t  = sum([c*vi*pi for (c,vi,pi) in zip((-8.0, -2.0),v,p)]) 
	t /= sum([c * vi**2 for (c,vi) in zip((4.0,1.0),v)])
	return [pi + vi * t for (pi, vi) in zip(p, v)]

def reflect(d,n):
	"""vector d reflection about unit vector normal n
	r = D - (2*dot(D,N) / abs(N)^2) * n"""
	# coefficent c, because easier
	c = 2 * dot(d,n)
	return [di - c * ni for (di, ni) in zip(d,n)]

def dot(x,y):
	"""Dot product of vectors x and y"""
	return sum([xi*yi for (xi,yi) in zip(x,y)])

def unit(x):
	"""Returns a unit vector in the same direction as x"""
	l = sum([i**2 for i in x])**0.5
	return [xi/l for xi in x]

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %( main())
	print "That took %f seconds" %(time() - start)