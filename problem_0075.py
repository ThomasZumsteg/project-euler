#!/usr/bin/python
"""http://projecteuler.net/problem=75"""

from time import time
from common import counter
from progressbar import ProgressBar

def main():
	L = 1500000
	p = ProgressBar()
	triangles = {}
	for m in p(range(2,int((L/2)**0.5))):
		for n in range((m%2)+1,min(m,int(L//(2*m)-m+1)),2):
#			print "m:%d, n:%d, k: " %(m,n),
			if not gcf(m,n) == 1:
#				print "gfc:%d" %(gcf(m,n))
				continue
			for k in range(1,L//(2*m*(m+n))+1):
#				print " %d" % k ,
				l = 2*k*m*(m+n)
				a = k*(m**2-n**2)
				b = k*(2*m*n)
				c = k*(m**2+n**2)
#				print "%d: (%d,%d,%d)" %(L,a,b,c)
				try: triangles[l] += 1
				except KeyError: triangles[l] = 1
#			print ""
	count = 0
	for k in triangles:
		if triangles[k] == 1:
			count += 1
	print "There are %d integer right triangles below %d" %(count, L)

def gcf(high,low):
	while low > 0:
		(high, low) = (low, high%low)
	return high

def brute():
	p = ProgressBar()
	L = 500
	triangles = {}
	for a in p(range(1,L)):
		top = (L**2-2*L*a)//(2*L-2*a)
		for b in range(a,top+1):
			c = is_square_int(a**2+b**2)
			if c:
				try: triangles[a+b+c].append((a,b,c))
				except KeyError: 
					triangles[a+b+c] = [(a,b,c)]
	for l in sorted(triangles.iterkeys()):
		print "%d: %s" %(l,str(triangles[l]))

def is_square_int(n):
	n_sqrt = int(n**0.5)
	if n_sqrt**2 == n:
		return n_sqrt
	else:
		return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
