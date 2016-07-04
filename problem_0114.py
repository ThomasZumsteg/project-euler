#!/usr/bin/python
"""http://projecteuler.net/problem=114"""

from time import time
from itertools import combinations
from progressbar import ProgressBar

def main():
	print fill(50,3)

def test():
	sizes = [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8]
	line = " m l"
	for t,i in zip(range(0,20),sizes):
		form = " %%%dd" %(i)
		line += form %(t)
	print line
	for m in range(1,19):
		line = "%2d: " %(m)
		for l,i in zip(range(m,m+19),sizes):
			form = " %%%dd" %(i)
			line += form % ways(l,m)
		print line

lookup = {}

def fill(l,m):
	if l < m:
		return 1
	global lookup
	if (l,m) in lookup:
		return lookup[(l,m)]
	else:
		w = fill_worker(l,m)
		lookup[(l,m)] = w
		return w

def fill_worker(l,m):
	count = 1
	for s in range(1,l-m+2):
		count += s * fill(l-m-s,m)
	return count

def ways(length, min_block):
	groups = set()
	for g in group(length, min_block):
		groups.add( group_or(g) )
	return len(groups) + 1

def group(l,m):
	items = [None] * (l-m+1)
	base = 2**m-1
	for i in range(len(items)):
		items[i] = base << i
	for n in range(1,len(items)+1):
		for c in combinations(items,n):
			yield c

def group_or(group):
	total = 0
	for g in group:
		total = total | g
	return total

def slow_ways(length, min_block):
	count = 1
	for n in range(min_block,length+1):
		for g in combinations(range(length),n):
			row = [False] * length
			for i in g:
				row[i] = True 
			if works(row, min_block):
				count += 1
	return count

def works(r, min_l):
	run = 0
	for v in r:
		if v:
			run += 1
		else:
			if run > 0 and run < min_l:
				return False
			run = 0
	return run == 0 or run >= min_l

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
