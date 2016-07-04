#!/usr/bin/python
"""http://projecteuler.net/problem=94"""

from time import time
from itertools import count
from sys import stdout

def main():
	[m,n] = [1,0]
	[bp,dp] = [None,None]
	done = False
	p_sum = 0
	big_num = 1000000000
	gamma = lambda m,n: m**2 + n**2
	beta  = lambda m,n: 2*m*n
	alpha = lambda m,n: m**2 - n**2
	p_alpha = lambda m,n: 2*gamma(m,n) + 2*alpha(m,n)
	p_beta  = lambda m,n: 2*gamma(m,n) + 2*beta(m,n)
	m_alpha = lambda n: (3*n**2+1)**0.5
	m_beta  = lambda n: (1+3*n**2)**0.5+2*n
	switch = True
	while not done:
		[m,n] = [n,m]
		for next_m, p in [(m_alpha, p_alpha),(m_beta,p_beta)]:
			m = next_m(n)
			perimeter = p(m,n)
			if switch:
				b = 2*alpha(m,n)
				c = gamma(m,n)
			else:
				b = 2*beta(m,n)
				c = gamma(m,n)
			s = ""
			if bp is not None: 
				d = (b - bp)**0.5
				s += " %8.2f " %(d)
			if dp is not None: s += " %8.6f" %(d/dp)
			try: dp = d
			except UnboundLocalError: pass
			bp = b
			print "(%9d, %9d, %9d)" %(c,c,b) + s
			switch = not switch
			if perimeter >= big_num:
				done = True
				break
			p_sum += perimeter
	print "Perimeter sum is %d" %( p_sum)

def main_slow():
	# triangle ccb
	# b = c + 1 or b = c - 1
	# b and c are integers
	# beta = b/2
	# Area A = ab/2 = beta * sqrt(3*beta^2+4*beta+1)
	#              or beta * sqrt(3*beta^2-4*beta+1)
	# a = c^2 - (b/2)^2
	#
	# Find integer solutions for sqrt(3*beta^2...
	# return (beta*2, beta*2+-1, beta*2+-1)
	p_sum = 0
	for b in count(3):
		for c in (b+1,b-1):
			if 2*c+b > 1000000: return
			a = (c**2 - b**2/4.0)**0.5
			A = 0.5 * a * b
			if int(A) == A:
				print "(%d,%d,%d) (%.2f) = %.2f"%(c,c,b,a,A)

def is_square(N):
	n = int(N**(0.5))**2
	if n == N: return True
	else: return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
