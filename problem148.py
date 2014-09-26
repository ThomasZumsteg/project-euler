#!/sru/bin/python
"""Finds the number of entries in the first billion rows of Pascal's triangle that are not divisible by 7"""

from time import time
from itertools import count
from sys import stdout
from math import log

def main():
	"""Solves problem (only for prime numbers)"""
	""" Triangle with entries not divisable by 3 marked (5,7,11,etc are similar)
                          x  |     |                 |  
                         x x |-sub triangle size p=0 ([( (3+1)*3 ) / 2]^1 entries)  
                        x x x|     |                 |  
                       x     x     |                 | 
                      x x   x x    |-sub triangle size p=1 ([6]^2 entries)
                     x x x x x x   |                 | 
                    x     x     x  |                 | 
                   x x   x x   x x |                 | 
                  x x x x x x x x x|                 | 
                 x                 x                 |
                x x               x x                |
               x x x             x x x               |
              x     x           x     x              |
             x x   x x         x x   x x             |
            x x x x x x       x x x x x x            |-sub triangle size p=2 ([6]^3 entries)
           x     x     x     x     x     x           |
          x x   x x   x x   x x   x x   x x          |
         x x x x x x x x x x x x x x x x x x         |
        x                 x                 x        |
       x x               x x               x x       |
      x x x             x x x             x x x      |
     x     x           x     x           x     x     |
    x x   x x         x x   x x         x x   x x    |
   x x x x x x       x x x x x x       x x x x x x   |
  x     x     x     x     x     x     x     x     x  |
 x x   x x   x x   x x   x x   x x   x x   x x   x x |
x x x x x x x x x x x x x x x x x x x x x x x x x x x|    
	Counts the number of entries by parsing triangle into triangles of triangles"""
	n = 7
	remaining_lines = 10**9

	# Greedy algorythm filles remaining lines with subsiquently smaller sub triangles
	entries = 0
	# largest triangle such that remaining_lines >= n ^ x
	largest_triangle = int(log(remaining_lines/n))
	sub_triangles = 1
	entries_per_triangle = ((n+1)*n)/2
	for p in reversed(range(largest_triangle+1)):
		rows_of_triangles = remaining_lines / (n**p)
		number_of_triangles = ((rows_of_triangles+1)*rows_of_triangles) / 2
		entries += number_of_triangles * entries_per_triangle**p * sub_triangles
		sub_triangles *= rows_of_triangles + 1
		remaining_lines %= n**p
	return entries

def draw_triangle(num, num_lines, f_name="problem148.txt"):
	"""Draws pascal's triangle in [f_name] for [num_lines] and puts an x on entries not divisable by [num]"""
	f = open(f_name, 'w')
	for n in range(num_lines):
		line = ""
		for k in range(n+1):
			if fact(n) / (fact(k) * fact(n-k)) % num == 0:
				line += "  "
			else: line += "x "
		f_string = "{:^%d}\n" %(num_lines*2)
		f.write("%3d" %(n) + f_string.format(line))
	f.close()

def fact(n):
	"""n factoral"""
	num = 1
	for i in range(2,n+1):
		num *= i
	return num

if __name__ == "__main__":
	start = time()
	print "Answer: %d" %(main())
	print "That took %f seconds" %(time() - start)