#!/usr/bin/python
"""http://projecteuler.net/problem=44"""

from math import sqrt
from common import counter

def main():
	p_gen = pent_gen(start=9)
	pent_list = [ 1, 5,12,22,35,51,70,92]
	for a in counter(3):
		for b in reversed(range(1,a)):
			delta  = pent_list[a] - pent_list[b]
			epsilon= pent_list[a] + pent_list[b]
			while epsilon > pent_list[-1]:
				pent_list.append(p_gen.next())
			if delta in pent_list and epsilon in pent_list:
				print "delta: %d\nbeta: %d" %(delta, pent_list[b])
				print "alpha: %d\nepsilon: %d" %(pent_list[a], epsilon)
				break

# delta < beta < alpha < epsilon

def pent_gen(lim=None,start=1):
	for i in counter(start):
		pent = i*(3*i-1)/2
		if lim and pent >= lim:
			break
		yield pent

#def main():
#	min_d = 100000000
#	for alpha in counter(2): 
##		print 2*alpha-2
##		if 3*alpha-2 > min_d:
##			break
#		a = alpha*(3*alpha-1)/2.0
#		for beta in reversed(range(1,alpha)):
#			b = beta *(3*beta -1)/2.0
#			epsilon = sqrt(1.0/36+2.0*(a+b)/3)+1.0/6
#			delta   = sqrt(1.0/36+2.0*(a-b)/3)+1.0/6
##			print "(%d:%d) (%f:%f)" %(a,b,epsilon,delta)
#			if epsilon - int(epsilon) < 1e-6 and delta - int(delta) < 1e-6:
##				min_d = int(delta)
#				print "P%d and P%d fit: %d" %(alpha, beta, min_d)
#				break
#	print min_d

if __name__ == "__main__":
	main()
