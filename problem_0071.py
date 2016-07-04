#!/usr/bin/python
"""http://projecteuler.net/problem=71"""

from time import time
import progressbar

def main():
	min_diff = None
	limit = 1000000
	(n,d) = (3,7)
	target = float(n)/d
	for denom in range(1,limit):
		if denom%d==0: continue
		numer = (denom * n) // d
		diff = target - float(numer) / denom
#		print "Target:%f - %d/%d = %f" %(target,numer,denom,diff)
		if not min_diff or min_diff[0] > diff:
#			print "New diff %d/%d: %f" %(numer, denom, diff)
			min_diff = (diff, numer, denom)
	print "%d/%d -> %d/%d" %(min_diff[1],min_diff[2],n,d)

def fract_gen(gran):
	pass

def test():
	queue = [(3,1),(2,1)]
	limit = 8
	branches = [ lambda (m,n):(2*m-n,m),
				 lambda (m,n):(2*m+n,m),
				 lambda (m,n):(m+2*n,n) ]
	while queue:
		node = queue.pop(0)
		print "%d/%d" % node[::-1]
		for branch in branches:
			new_node = branch(node)
			if max(new_node) <= limit:
				queue.append(new_node)

if __name__ == "__main__":
	start = time()
	main()
#	test()
	print "That took %f seconds" %(time()-start)
