#!/usr/bin/python
"""Finds the probability of selecting a single sheet of a5 paper starting witha single sheet of a1 paper and subdividing until an a5 is available
Details: https://projecteuler.net/problem=151"""

from time import time
from random import choice
from sys import stdout

def main():
	"""Wrapper for jobs_branches"""
	# Runs jobs_branches and rounds to 6 decimal places
	return round(jobs_branches(), 6)

def jobs_branches():
	"""Finds the probability of single sheet envelopes by exploring each possability (branch)"""
	# Index of [paths] is the batch number
	# Each batch has a dictionary 
	# Keys: envelopes and values: Probability of getting that envelope
	# E.G. the first batch (paths[0]) has one envelope (1,) and has the probaility of 1.0
	#      the seconds batch (paths[1] has one envelope (2,3,4,5,) and has probabilty 1.0
	paths = [{(1,):1.0}]
	# Continues until there is only one sheet of A5 paper left
	while (5,) not in paths[-1]:
		# Generates next batch (out_paths) from current bath (in_paths)
		in_paths = paths[-1]
		paths.append({})
		out_paths = paths[-1]
		for sheets, prob in in_paths.iteritems():
			# Probability of getting from current node to next node
			update_prob = prob / len(sheets)
			for s in sheets:
				# Generates next node by replacing sheet (s) with (s-1, s-2 ... 5)
				copy = list(sheets)
				copy.remove(s)
				if s != 5: copy.extend(range(s+1,6))
				copy.sort()
				try: out_paths[tuple(copy)] += update_prob
				except KeyError: out_paths[tuple(copy)] = update_prob
	# Sums probability of getting a single sheet envelope
	total = 0.0
	for d in paths:
		for k in ((2,), (3,), (4,)):
			try: total += d[k]
			except KeyError: pass
	return total

def brute():
	"""Simulates the problem, not precise but good to 2-3 decimal places"""
	# Number of times a single sheet envlope is encountered
	one_sheets = 0
	times = 100000000
	for n in range(1,times+1):
		one_sheets += sheet_gen()
		stdout.write("%10.7f\r" %(one_sheets/float(n)))
		stdout.flush()
	return float(one_sheets) / times

def sheet_gen():
	"""Simulates a single week and returns the number of single sheet envelopes encountered"""
	count = 0
	# Second batch
	sheets = [2,3,4,5]
	# Ends on 15th batch, latch batch is always [5,]
	for _ in range(13):
		s = choice(sheets)
		sheets.remove(s)
		if s != 5: sheets.extend(range(s+1, 6))
		if len(sheets) == 1: count += 1
	return count
		
if __name__ == "__main__":
	"""Scriptify and time [main]"""
	start = time()
	print "Answer: %f" %(main())
	print "That took %f seconds" %( time() - start)