#!/usr/bin/python
"""Finds the greatest sum of adjacent entries in any direction for a 2000x2000 grid of numbers genterated using a lagged fibonacci generator"""
""" Generator:
	 1 <= k <=    55,   s[k] = { 100003 - 200003*k + 300007*k^3} (modulo 1000000) - 500000
    56 <= k <= 4000000, s[k]=  {s[k-24] +  s[k-55] +    1000000} (modulo 1000000) - 500000
"""

from time import time
from sys import stdout

def main():
	"""Wrapper for max_sub_array"""
	return max_sub_array(lagged_fib_generator, 2000)

def quick_gen():
	"""Test grid for the problem"""
	grid = [-2,5,3,2,9,-6,5,1,3,2,7,3,-1,8,-4,8]
	for e in grid:
		yield e

def max_sub_array(gen, size):
	"""Finds the maxiumum sum sub arrays of adjacent elements in a square array
	gen is a generator that gives numbers sequentially,
	the array is [size] x [size]"""
	# Stores the state for each direction
	rows      = [[None, None] for _ in range(size)]
	cols      = [[None, None] for _ in range(size)]
	diags     = [[None, None] for _ in range(size * 2)]
	anti_diags= [[None, None] for _ in range(size * 2)]
	for k, n in enumerate(gen()):
		# for each new number relevent row, column, diagonal, and anti-diagonal is update
		row_r       = k // size
		col_c       = k % size
		diag_d      = size - 1 + col_c - row_r
		anti_diag_a = row_r + col_c
		# Exit condition 
		if row_r >= size: break
		#stdout.write("row: %5d, col: %5d\r" %(row_r, col_c))
		#stdout.flush()
		#Updating done here
		for state in [rows[row_r], cols[col_c], diags[diag_d], anti_diags[anti_diag_a]]:
			update_kadean(state,n)
	# Find the largest in each direction and return the largest of those
	f = lambda a: max([e[1] for e in a])
	return max([f(rows), f(cols), f(diags), f(anti_diags)])

def update_kadean(state, n):
	"""Implementation of Kadeans algorythm where state and new element (n) are passed in
	state contains (max_ending _here, max_so_far)
	See: http://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane.27s_algorithm"""
	if state[0] is None:
		# Initilization
		(state[0], state[1]) = (n, n)
	else:
		state[0] = max(n, state[0] + n)
		state[1] = max(state[0], state[1])

def find_largest_brute(grid):
	"""Brute forces the largest sub array of adjacent elements in grid"""
	largest = grid[0][0]
	for i, row in enumerate(grid):
		for j, element in enumerate(row):
			stdout.write("(%4d, %4d): %d\r" %(i,j,largest))
			stdout.flush()
			for gen in [row_gen, col_gen, diag_gen, anti_diag_gen]:
				val = 0
				for diff in gen(grid,i,j):
					val += diff
					largest = max(largest, val)
	stdout.write("\n")
	stdout.flush()
	return largest

def row_gen(grid,x,y):
	"""Yields elements in row x starting from column y"""
	for j in range(y,len(grid[x])):
		yield grid[x][j]

def col_gen(grid,x,y):
	"""Yields elements in column y starting with row y"""
	for i in range(x,len(grid)):
		yield grid[i][y]

def diag_gen(grid,x,y):
	"""Yields elements in diagonal starting at x,y"""
	s = min(len(grid) - x, len(grid[x]) - y)
	for d in range(s):
		yield grid[x+d][y+d]

def anti_diag_gen(grid,x,y):
	""""Yields elemets in anti-diagonal starting at x,y"""
	s = min(x, len(grid[x]) - y)
	for d in range(s):
		yield grid[x-d][y+d]

def lagged_fib_generator():
	"""Lagged fibonacci generator required for the problem, yields elements sequentially"""
	l = 55
	f = lambda k: ((100003 - 200003*k + 300007*k**3) % 1000000) - 500000
	nums = map(f, range(1,l+1))
	i = 0
	while True:
		yield nums[i]
		nums[i] = ((nums[i-24] + nums[i-55] + 1000000) % 1000000) - 500000
		i = (i + 1) % l

def build_grid(size):
	"""Builds grid of [size]x[size] from lagged_fib_generator"""
	num_gen = lagged_fib_generator()
	grid = [[0] * size for _ in range(size)]
	for i in range(size):
		for j in range(size):
			grid[i][j] = num_gen.next()
	return grid

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %(main())
	print "That took %f seconds" %(time() - start)