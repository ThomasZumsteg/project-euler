#!/usr/bin/python

from time import time
from sys import stdout
from itertools import combinations

def main():
	line_sum = 0
	f_name = "problem105.txt"
	for i,line in enumerate(open(f_name)):
		line = [int(x) for x in line.split(',')]
		stdout.write("%d: %d \r" %(i, len(line)))
		stdout.flush()
		if is_special(line):
			line_sum += sum(line)
	stdout.flush()
	print "The sum of all special lines is %d" % line_sum

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
