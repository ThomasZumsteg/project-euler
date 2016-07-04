#!/usr/bin/python
"""http://projecteuler.net/problem=107"""

from time import time
from sys import stdout
from progressbar import ProgressBar

def main():
	f_name = "problem107.txt"
	network = read_network(f_name)
	old_sum = sum_network(network)
	new_sum = minimum_net(network)
	print "Improved %d - %d = %d" %(old_sum, new_sum, old_sum - new_sum)

def minimum_net(N):
	min_sum = 0
	p = ProgressBar()
	for i in p(range(1,len(N))):
		low = min_potential(N,i)
		min_sum += low
#		print "%d: %d" %(i, low)
	return min_sum

def min_potential(N,i):
	row_mins = []
	for line in N[i:]:
		row_mins.append(min(line[:i]))
	low = min(row_mins)
	j   = row_mins.index(low) + i
	swap(N,i,j)
	return low

def swap(N,i,j):
#	quick_print(N)
#	print '-' * 40
	for n in [x for x in range(len(N)) if x not in [i,j]]:
		tmp = (N[n][i],N[n][j])
		(N[n][j],N[n][i]) = tmp
		(N[j][n],N[i][n]) = tmp
#	quick_print(N)

def get_column(net,c):
	col = []
	for line in net:
		col.append(line[c])
	return col

def sum_network(N):
	net_sum = 0
	for i in range(len(N)):
		for e in N[i][i:]:
			if e != '-':
				net_sum += e
	return net_sum

def read_network(f_name):
	network = []
	for line in open(f_name):
		line = line.strip().split(',')
		line = [x if x=='-' else int(x) for x in line]
		network.append(line)
	return network

def quick_print(N):
	for line in N:
		line = [' -' if x == '-' else str(x) for x in line]
		print ' '.join(line)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)

"""
   A  B  C  D  E  F  G
A  - 16 12  -  -  -  -
B 16  -  - 17  -  -  -
C 12  -  -  -  -  -  -
D  - 17  -  - 18 19  -
E  -  -  - 18  -  - 11
F  -  -  - 19  -  -  -
G  -  -  -  - 11  -  -
"""
