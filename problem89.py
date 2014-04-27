#!/usr/bin/python
"""http://projecteuler.net/problem=89"""

from time import time
from re import match

def main():
	digits = [ ('I',    1),
			   ('IV',   4),
			   ('V',    5),
			   ('IX',   9),
			   ('X',   10),
			   ('XL',  40),
			   ('L',   50),
			   ('XC',  90),
			   ('C',  100),
			   ('CD', 400),
			   ('D',  500),
			   ('CM', 900),
			   ('M', 1000) ]
	char_saved = 0
	for line in open("problem89.txt"):
		line = line.strip()
		num = roman_to_int(line, digits)
		short_r = int_to_roman(num, digits)
		char_saved += len(line) - len(short_r)
	print "%d characters saved" %(char_saved)

def roman_to_int(roman, digits):
	total = 0
	for digit, val in reversed(digits):
		m = match("(" + digit + ")+", roman)
		if m is None: continue
		roman = roman[m.end():]
		total += val * (m.end() // len(digit))
	return total

def int_to_roman(num, digits):
	s = ""
	for digit, val in reversed(digits):
		s += digit * (num // val)
		num = num % val
	return s


if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start )
