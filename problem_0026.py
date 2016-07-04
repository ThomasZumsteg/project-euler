#!/usr/bin/python
"""A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

	1/2	= 	0.5
	1/3	= 	0.(3)
	1/4	= 	0.25
	1/5	= 	0.2
	1/6	= 	0.1(6)
	1/7	= 	0.(142857)
	1/8	= 	0.125
	1/9	= 	0.(1)
	1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part."""

from common import counter

def main():
	longest_cycle = 0
	denom = None
	for i in range(2,1000):
		cycle = div_cycle(1,i)
		if cycle > longest_cycle:
			longest_cycle = cycle
			denom = i
#			print "%d cycles %d times" %(i, cycle)
	print denom

def div_cycle(num,denom):
	remainer = num
	digits    = []
	remainers = []
	while remainer:
		digit = 10*remainer/denom
		remainer = (remainer*10) % denom
		itter = zip(reversed(digits), reversed(remainers), counter(1))
		for val, rem, count in itter:
			if val == digit and rem == remainer:
				return count
		remainers.append(remainer)
		digits.append(digit)
	return 0

if __name__ == "__main__":
	main()
#	print div_cycle(1,499)
