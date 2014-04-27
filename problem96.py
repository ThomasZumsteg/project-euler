#!/usr/bin/python
"""http://projecteuler.net/problem=96"""

from time import time
from sys import stdout

def main():
	sudoku_sum = 0
	file_name = "problem96.txt"
	for grid,sudoku in read_sudoku(file_name):
		stdout.write("Solving %s\r" % grid)
		stdout.flush()
#		print sudoku
		s = solve_sudoku(sudoku)
#		print s
		sudoku_sum += int(s)
	print "Sudoku sum is %d" %(sudoku_sum)


def solve_sudoku(sudoku):
	i = 0
	solving = sudoku[:]
#	print sudoku
	while i < 81 and i >= 0:
#		print "%d:%d" %(i,solving[i]),
		if valid(solving, i):
			try: i = next_element(solving, i)
			except IndexError: break
		else:
			if solving[i] < 9:
				solving[i] += 1
			else:
				i = backup(sudoku, solving, i)
#				print ""
				solving[i] += 1
#	print solving
	return ''.join([str(x) for x in solving[:3]])
		
def next_element(sudoku, i):
	while sudoku[i] != 0:
		i += 1
	return i

def backup(sudoku, solving, i):
#	print "\nBacking up:",
	while i >= 0 and i < 81:
#		print "%d:"%i,
		if sudoku[i] != 0:
#			print "Set",
			i -= 1
		elif solving[i] >= 9:
#			print "Over (%d)" %(solving[i]),
			solving[i] = 0
			i -= 1
		else:
			break
	return i

def valid(sudoku, i):
	if sudoku[i] == 0: return False
	row = i // 9
	col = i  % 9
	box = (row//3) * 3 + col//3
	row_e = range(9*row,9*row+9)
	col_e = range(col,81,9)
	box_e = [27*(box//3)+3*(box%3)+e for e in 
			 [0,1,2,9,10,11,18,19,20]]
#	print [row_e, col_e, box_e]
	for elements in [row_e, col_e, box_e]:
		seen = []
		for e in elements:
			if sudoku[e] == 0: continue
			if sudoku[e] in seen:
#				print elements
				return False
			seen.append(sudoku[e])
	return True

def read_sudoku(f_name):
	breaks = r"Grid /d/d"
	f = open(f_name)
	while True:
		s = f.readline().strip()
		sudoku = []
		for i in range(9):
			line = f.readline().strip()
			if not line: return
			sudoku += [int(c) for c in line]
		yield s, sudoku

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
