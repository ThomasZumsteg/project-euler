#!/usr/bin/python
"""Finds the number of rectangles that could be situated in a cross hatched grid of dimentions 47x43 and smaller
Details: https://projecteuler.net/problem=147"""

from time import time
from sys import stdout

def main():
	num_subs = 0
	# Steps though all grids of valid size
	for y in range(1,44):
		for x in range(1,48):
			#          diagonals       + normaly oriented (x*(x+1)/2) * (y*(y+1)/2)
			this_sub = count_subs(x,y) + ((x*y) * (x+1) * (y+1)) / 4
			num_subs += this_sub
			# Prints updates
			stdout.write("(%2d, %2d): %d\r" %(x,y,this_sub))
			stdout.flush()
	return num_subs

def count_subs(x,y):
	"""Counts all the diagonal sub rectangles in a cross-hatched rectangle of size x,y"""
	# Encases diagonals in square grid of size 'square'
	square = x + y - 2
	subs = 0
	# For every point counts the number of rectagles with (a,b) as upper left corner
	for a in range(square):
		for b in range(square):
			if valid(a,b,x,y):
				this_subs = subs_at_point(a,b,x,y)
				print "%3d " %(this_subs),
			print ""
	return subs

def subs_at_point(a,b,x,y):
	"""Counts all the rectangles in grid of size x,y that start with the upper left corner at a,b"""
	max_i = x + y
	subs = 0
	# Counts left -> right, top -> bottom
	for j in range(b, x+y):
		# stops when it can't go down anymore
		if not valid(a,j,x,y): return subs
		for i in range(a,max_i):
			if valid(i,j,x,y): subs += 1
			else:
				max_i = min(max_i, i)
				break

def valid(a,b,x,y):
	"""Checks if point (a,b) on square grid is within rectagle x,y drawn diagonal to the grid"""
	# Size of the square grid that encases rectagle x,y
	square = x + y - 2
	# Taxi cab distance (no diagonals) from (p_a, p_b) to (a,b)
	steps = lambda p_a, p_b: abs(p_a - a) + abs(p_b - b)
	# Top/Bottom/Left/Right bound
	if min(a,b) < 0 or max(a,b) >= square: return False
	# Upper left/Lower right corner check
	if steps(0,0) < (x - 2) or steps(square - 1, square - 1) < (x - 2): return False 
	# Lower left/Upper right corner check
	elif steps(square - 1, 0) < (y - 2) or steps( 0, square - 1) < (y - 2): return False
	return True

if __name__ == "__main__":
	start = time()
	print "Answer : %d" %(main())
	print "That took %f seconds" %(time() - start)