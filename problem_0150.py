#!/usr/bin/python
"""Find the smallest possible sub-triangle sum in a triangular array of integers generated using a Linear Conguential Generator"""
"""Generator:
t := 0
for k = 1 up to k = 500500:
	t := (615949*t + 797807) % 2^20
	s_k := t - 2^ 19
"""
from time import time
from itertools import count
from sys import stdout

def main():
	"""Builds triangle then finds smallest sum for every row"""
	triangle = build_triangle(lc_gen(500500))
	smallest = triangle[0][0]
	for i in range(2,len(triangle)+1):
		smallest = min(smallest, smallets_in_row(triangle[:i]))
	return smallest

def smallets_in_row(triangle):
	"""Finds the smallest sub triangle sum such that the sub triangle ends on the last line"""
	r = len(triangle)
	prev = [0] * (r+1)
	cur = triangle[r-1][:]
	smallest = min(cur)
	for i in reversed(range(r-1)):
		next = triangle[i][:]
		for j in range(i+1):
			# Addds two adjacent sub triangles
			next[j] += cur[j] + cur[j+1]
			# subtacts the common area
			next[j] -= prev[j+1]
		(cur, prev) = (next, cur)
		smallest = min(smallest, min(cur))
	return smallest

def build_triangle(gen):
	"""Builds the Triangle as an array of arrays of increasing size"""
	triangle = []
	for row in row_gen(gen):
		triangle.append(row)
	return triangle

def row_gen(gen):
	"""Generates rows to build a triangle"""
	for row_length in count(1):
		try: row = [gen.next() for _ in range(row_length)]
		except StopIteration: return
		yield row

def lc_gen(lim):
	"""Linear Congruential Generator from problem statement"""
	t = 0
	for k in range(1,lim+1):
		t = (615949 * t + 797807) % 2**20
		yield t - 2**19

def brute(gen):
	"""Brute force smallest sub triangle by testing all sub triangles"""
	triangle = build_triangle(gen)
	smallest = triangle[0][0]
	for r, row in enumerate(triangle):
		for e, n in enumerate(row):
			for size in range(r-e+1):
				st = sub_triangle_sum(r,e,size,triangle)
				point = (r + 1) * r // 2 + e
				#stdout.write("%6d: (%5d, %5d, %5d): %d\r" %(point,r,e,size, st))
				#stdout.flush()
				smallest = min(smallest, st)
	return smallest

def sub_triangle_sum(row, element, size, triangle):
	"""The sum of a sub triangle within [triangle] with the lower left corner at [element] in [row] containing [size] rows"""
	if row - element < size: raise SizeError("Sub triangle to large")
	s = 0
	for i,r in enumerate(range(row-size, row+1),1):
		start = element
		end = element + i
		#print "Adding: %s" %(triangle[r][start:end])
		s += sum(triangle[r][start:end])
	return s

class SizeError(Exception):
	"""Error if generator ends in the middle of a row"""
	def __init__(self, value):
		self.value = value
	def __str__(self):
		return repr(self.value)

def quick_gen():
	"""Test generator, triangle with 6 rows, smallest sub is -42"""
	t = [15,-14,-7,20,-13,-5,-3,8,23,-26,1,-4,-5,-18,5,-16,31,2,9,28,3]
	for n in t:
		yield n

def kadane_triangle(gen):
	"""Experiment, didn't work. Attempted to adapt kadane's algrythm for triangles"""
	prev = []
	min_so_far = None
	for r,row in enumerate(row_gen(gen)):
		prev = update_row(row, prev)
		if min_so_far is None:
			min_so_far = min(prev)
		else:
			min_so_far = min(min(prev), min_so_far)
		stdout.write("%4d: %d\r" %(r, min_so_far))
		stdout.flush()
	stdout.write("\n")
	stdout.flush()
	return min_so_far

def update_row(row, prev):
	"""Method for kadane_triangle"""
	length = len(row)
	vals = []
	p = 0
	for i in range(length):
		vals.append(row[i])
		for j in range(i+2, length+1):
			#print "%2d: %s + %d" %(len(vals), row[i:j], prev[p])
			vals.append(sum(row[i:j]) + prev[p])
			p += 1
	return vals

if __name__ == "__main__":
	"""Scriptify and time main"""
	start = time()
	print "Answer: %d" %(main())
	print "That took %f seconds" %(time() - start)