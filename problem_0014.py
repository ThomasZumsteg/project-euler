#!/usr/bin/python
"""The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million"""

def main():
	max_chain = (None, None)
	for start in range(1,int(1e6)+1):
		i = 0
		for num in collatz_seq_gen(start):
			i += 1
		if max_chain[1] < i:
			max_chain = (start, i)
		if start % 10000 == 0:
			print "%d: %d " %(max_chain[0], max_chain[1])
	print max_chain

def collatz_seq_gen(start):
	while True:
		yield start
		if(start == 1):
			break
		elif(start % 2 == 0):
			start /= 2
		else:
			start = start * 3 + 1

if __name__ == "__main__":
   main() 
