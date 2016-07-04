#!/usr/bin/python
"""http://projecteuler.net/problem=86"""

from time import time
import sys

def main():
	count = 0
	x = 1
	goal = 1000000
	while count < goal:
		sys.stdout.write("%5d: %10d\r" %(x, count))
		sys.stdout.flush()
		x += 1
		for yz in range(1,2*x+1):
			if is_int(yz**2 + x**2):
				# x >= y >= z
				# x**2 + (y+z)**2 is integer
				count += ways_to_make(yz,x)
#		print "%5d: %10d" %(x, count)
#		print "%5d: %10d - brute" %(x, brute_routes(x))
	print "%5d: %10d" %(x, count)


#def routes(m):
#	total = 0
#	for x in range(int(m/(5**0.5)),m-3):
#		N = m**2-x**2
#		if is_int(N):
#			total += ways_to_make(N**0.5, x)
#	return total

def is_int(N):
	if (int(N**0.5))**2 == N:
		return True
	else:
		return False

def ways_to_make(n,m):
	if n <= (m+1):
		return n//2
	elif 2*m+1 >= n:
		return (2*m-n)//2+1
	else:
		return 0

def brute_routes(M):
	count = 0
	for x in range(1,M+1):
		for y in range(1,x+1):
			for z in range(1,y+1):
				path = shortest_path(x,y,z)
				if path % 1 == 0.0:
					count += 1
	return count

def shortest_path(x,y,z):
	nums = sorted([x,y,z])
	n = nums.pop()
	m = sum(nums)
	return (n**2+m**2)**0.5

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() -start)
