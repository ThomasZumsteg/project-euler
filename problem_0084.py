#!/usr/bin/python
"""http://projecteuler.net/problem=84"""

from collections import defaultdict
import random
from time import time

def main():
	names = ["GO  ", "A1  ", "CC1 ", "A2  ", "T1  ", "R1  ", "B1  ", "CH1 ", "B2  ", "B3  ",
			"JAIL", "C1  ", "U1  ", "C2  ", "C3  ", "R2  ", "D1  ", "CC2 ", "D2  ", "D3  ",
			"FP  ", "E1  ", "CH2 ", "E2  ", "E3  ", "R3  ", "F1  ", "F2  ", "U2  ", "F3  ",
			"G2J ", "G1  ", "G2  ", "CC3 ", "G3  ", "R4  ", "CH3 ", "H1  ", "T2  ", "H2  "]
	n_dice = 2
	board = build_board(n_dice)
	p_space = guass_elimination(board)
	s = ""
	for e in reversed(sorted(p_space[:-2])):
		i = p_space.index(e)
		s += "%2d: %s: %5.2f\n" %(i, names[i], e * 100 )
	print s

def guass_elimination(A):
	rows = range(len(A))
	cols = range(len(A[0]))
	for k in cols[:-1]:
		col_vals = [A[i][k] for i in rows[k:]]
		i_max = col_vals.index(max(col_vals, key=abs)) + k
		if A[i_max][k] == 0.0:
			raise Exception("Matrix is singular")
		[A[k],A[i_max]] = [A[i_max],A[k]]
		A[k] = [A[k][n]/float(A[k][k]) for n in cols]
		for i in rows:
			if i == k: continue
			A[i] = [A[i][n] - A[i][k] * A[k][n] for n in cols]
	return [A[n][-1] for n in rows]

def build_board(sides):
	board = [ [] for x in range(40) ]
	for start in range(40):
		for i, val in enumerate(space_prob(sides, start)):
			board[i].append(val)
	# set up for solve
	for i in range(len(board)):
		board[i][i] -= 1
		board[i] += [0]
	board += [[1] * (41)]
	return board

def space_prob(sides, start):
	# one less then the number of doubles that goes to jail
	JAIL = 10
	spaces = [0] * 40
	queue = roll_dice(sides, start)
	while queue:
		node = queue.pop()
		p = 12.0
		if node.was_double:
			spaces[JAIL] += p/3
			p *= 2/3
		space_handler(spaces, node.space, p)
	s = float(sum(spaces))
	return [e/s for e in spaces]

def space_handler(spaces, s, p):
	G2J       = [30]
	CC_spaces = [2, 17, 33]
	CH_spaces = [7, 22, 36]
	R_map  = {7:15, 22:25, 36:5}
	U_map  = {7:12, 22:28, 36:12}
	JAIL = 10
	GO   = 00
	if s in CC_spaces:
		spaces[GO]  +=  p/16
		spaces[JAIL]+=  p/16
		spaces[s]   += (p*14)/16
	elif s in CH_spaces:
		spaces[GO]  +=  p/16
		spaces[JAIL]+=  p/16
		spaces[11]  +=  p/16 # C1
		spaces[24]  +=  p/16 # E3
		spaces[39]  +=  p/16 # H2
		spaces[05]  +=  p/16 # R1
		spaces[s-3] +=  p/16
		spaces[s]   +=  (6*p)/16
		spaces[R_map[s]] += (2*p)/16
		spaces[U_map[s]] += p/16
	elif s in G2J:
		spaces[JAIL] += p
	else:
		spaces[s] += p

class New_node(object):
	def __init__(self, space, double):
		self.space        = space
		self.was_double   = double

def roll_dice(sides, offset):
	queue = []
	for m in range(sides):
		for n in range(sides):
			space = (m+n+offset+2) % 40
			if m == n: double = True
			else:      double = False
			node = New_node(space, double)
			queue.append(node)
	return queue

def print_board(b):
	for line in b:
		s = ""
		for e in line:
			s += "%3.0f " %(100*e)
		print s

def brute():
	board = [0] * 40
	pos = 0
	n_double = 0
	for i in range(1000000):
		m = random.choice([1,2,3,4,5,6])	
		n = random.choice([1,2,3,4,5,6])	
		if m == n: n_double += 1
		else: n_double = 0
		if n_double >= 3:
			pos = 10
			n_double = 0
		else:
			pos = (pos + m + n) % 40
			pos = special_moves(pos)
		board[pos] += 1
	names = ["GO  ", "A1  ", "CC1 ", "A2  ", "T1  ", "R1  ", "B1  ", "CH1 ", "B2  ", "B3  ",
			"JAIL", "C1  ", "U1  ", "C2  ", "C3  ", "R2  ", "D1  ", "CC2 ", "D2  ", "D3  ",
			"FP  ", "E1  ", "CH2 ", "E2  ", "E3  ", "R3  ", "F1  ", "F2  ", "U2  ", "F3  ",
			"G2J ", "G1  ", "G2  ", "CC3 ", "G3  ", "R4  ", "CH3 ", "H1  ", "T2  ", "H2  "]
	board_sum = float(sum(board))
	p_space = [x/board_sum for x in board]
	s = ""
	for e in reversed(sorted(p_space)):
		i = p_space.index(e)
		s += "%2d: %s: %5.2f\n" %(i, names[i], e * 100 )
	print s

def special_moves(s):
	G2J       = [30]
	CC_spaces = [2, 17, 33]
	CH_spaces = [7, 22, 36]
	R_map  = {7:15, 22:25, 36:5}
	U_map  = {7:12, 22:28, 36:12}
	if s in CH_spaces:
		array = [0,10,11,24,39,05,s-3] + 6*[s]
		array += [R_map[s]]*2 + [U_map[s]]
		s = random.choice(array)
	if s in CC_spaces:
		s = random.choice([00,10]+[s]*14)
	if s in G2J:
		s = 10
	return s

if __name__ == "__main__":
	start = time()
#	main()
	brute()
#	board = build_board(6)
#	print_board(board)
	print "That took %f seconds" %( time() - start)
