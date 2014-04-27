#!/usr/bin/python
"""http://projecteuler.net/problem=82"""

from time import time
import sys

class Node(object):
	def __init__(self, val, links):
		self.value = val
		self.links = links
		self.new   = True
	def __iter__(self):
		for link in links:
			yield link

def main():
	matrix = read_matrix("problem82.txt")
	queue = []
	for i in range(80):
		queue.append((0,(i,0)))
	while True:
		sys.stdout.write("The mininal path is: %d\r" %(queue[-1][0]))
		sys.stdout.flush()
		(val, link) = queue.pop()
		matrix[link].new = False
		val += matrix[link].value
		if link[1] >= 79:
			print "The minimal path is: %d" % val
			return
		for l in matrix[link].links:
			if l in matrix and matrix[l].new:
				insert( (val, l), queue)

def insert(node, queue):
	i = 0
	while i < len(queue):
		if queue[i][0] <= node[0]:
			break
		i += 1
	queue[i:i] = [node]

def read_matrix(fh):
	matrix = {}
	for l, line in enumerate(open(fh)):
		for i, n in enumerate(line.split(',')):
			up    = (l-1, i  )
			down  = (l+1, i  )
			right = (l  , i+1)
			links = (up, down, right)
			matrix[(l,i)] = Node(int(n),links)
	return matrix

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
