#!/usr/bin/python
"""http://projecteuler.net/problem=81"""

from time import time
import sys

class Node(object):
	def __init__(self, value, down, right):
		self.value = value
		self.links = (down, right)
		self.visited = False
	def __iter__(self):
		for item in self.links:
			yield item
	def __repr__(self):
		args = (self.value, self.right, self.down)
		return "Val: %s, R: %s, D: %s" % args
	def __str__(self):
		args = (self.value, self.right, self.down)
		return "Val: %s, R: %s, D: %s" % args

def main():
	matrix = read_matrix()
	print "Read in"
	queue = [(0, (0,0))]
	while True:
		(val, address) = queue.pop()
		node = matrix[ address ]
		node.visited = True
		val += node.value
		has_link = False
		for link in node:
			if link in matrix:
				has_link = True
				if not matrix[link].visited:
					insert( (val, link), queue)
		if not has_link:
			print "The lowest path val is %d" % val
			return
		sys.stdout.write(str(len(queue))+'\r')
		sys.stdout.flush()

def insert(node, queue):
	i = 0
	while i < len(queue):
		if queue[i][0] <= node[0]:
			break
		i += 1
	queue[i:i] = [node]

def read_matrix():
	matrix = {}
	read_file = "problem81.txt"
	for l, line in enumerate(open(read_file)):
		for i, n in enumerate(line.split(',')):
#			print "%4d" % int(n),
			down  = (l + 1, i)
			right = (l, i + 1)
			matrix[(l,i)] = Node(int(n), down, right)
#		print ""
	return matrix

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
