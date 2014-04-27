#!/usr/bin/python
"""http://projecteuler.net/problem=109"""

from time import time
from itertools import combinations_with_replacement

def main():
	ways = 0
	points = build_points()
	for n in range(1,100):
		w = checkouts(n, points)
		ways += w
	print "There are %d ways to checkout" %(ways)

def build_points():
	points = [set() for x in range(121)]
	slices = []
	for i in range(1,21) + [25]:
		for m,t in [(1,'S'),(2,'D'),(3,'T')]:
			slices.append(( m*i , t+str(i) ))
			# Catches one treble bull
			if i == 25 and m == 3: continue
			points[m*i].add(("", t+str(i)))
	slices.pop() # removes treble 25
	for a,b in combinations_with_replacement(slices, 2):
		total = a[0] + b[0]
		names = tuple(sorted([a[1],b[1]]))
		points[total].add(names)
	points[0].add(("","")) # 0 points is done
	return points

def checkouts(n, points):
	ways = 0
	for d in range(1,21) + [25]:
		r = n - 2 * d
		if r < 0 or r >= len(points): continue
		ds = 'D' + str(d)
		for a,b in points[r]:
#			print "%.3s %.3s %.3s" %(ds,b,a)
			ways += 1
	return ways

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
