#!/usr/bin/python
"""http://projecteuler.net/problem=103"""

from time import time
from itertools import combinations
from sys import stdout

def main():
	root = (21,32,39,40,41,43,46)
	for A in gen_A(6, root):
#		print "%d: %s" %(sum(A), A)
		stdout.write("%d: %s\r" %(sum(A), A))
		stdout.flush()
		if is_special(A):
			print "%d: %s" %(sum(A),str(A))

def gen_A(size, root):
	queue = set()
	queue.add(root)
	while queue:
		done = set()
		for n in queue:
			for m in process_node(n):
				if m not in done:
					yield m
				done.add(m)
		queue = done

def process_node(node):
	for i in range(len(node)):
		new = list(node)
		new[i] -= 1
		if no_duplicates(new) and min(new) > 0:
			yield tuple(new)

def no_duplicates(node):
	if len(set(node)) == len(node):
		return True
	else:
		return False

def is_special(A):
	A = list(A)
	for B,C in split(A):
#		print "B: %s, C: %s" %(str(B), str(C))
		(B_sum, B_len) = (sum(B), len(B))
		(C_sum, C_len) = (sum(C), len(C))
		if B_sum == C_sum:
			return False
		elif (B_sum > C_sum) and (B_len < C_len):
			return False
		elif (B_sum < C_sum) and (B_len > C_len):
			return False
	return True

def split(A):
	for n in range(1,len(A)//2+1):
#		print "Testing %d" %( n)
		for B in combinations(A,n):
			Cs = [x for x in A if x not in B]
			for n in range(1,len(Cs)+1):
				for C in combinations(Cs,n):
					yield B,C

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
