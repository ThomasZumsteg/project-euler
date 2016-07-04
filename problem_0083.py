#!/usr/bin/python
"""http://projecteuler.net/problem=83"""

from time import time
import sys

class Node(object):
	def __init__(self, val, links):
		self.val = val
		self.links = links
		self.new = True
	def __iter__(self):
		for l in self.links:
			yield l

def main():
	string = "The minimal path length is: %d"
	matrix = read_matrix("problem83.txt")
	queue = [(0,(0,0))]
	while True:
		(val, link) = queue.pop()
		sys.stdout.write(string % val + "\r")
		sys.stdout.flush()
		matrix[link].new = False
		val += matrix[link].val
		if link == (79, 79):
			print string % val
			return
		for l in matrix[link].links:
			if l in matrix and matrix[l].new:
				insert((val,l), queue)

def insert(node, queue):
	i = 0
	while i < len(queue):
		if queue[i][0] <= node[0]:
			break
		i += 1
	queue[i:i] = [node]

def print_matrix(matrix):
	string = ''
	for l in range(16):
		for i in range(16):
			if matrix[(l,i)]:
				string += '+'
			else:
				string += '*'
		string += '\n'

def read_matrix(fh):
	matrix = {}
	for l, line in enumerate(open(fh)):
		for i, n in enumerate(line.split(',')):
			links = set()
			links.add( (l-1, i  ) ) # up
			links.add( (l+1, i  ) ) # down
			links.add( (l  , i-1) ) # left
			links.add( (l  , i+1) ) # right
			matrix[(l,i)] = Node( int(n), links )
	return matrix

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
