#!/usr/bin/python
"""http://projecteuler.net/problem=42"""

from re import sub
from math import sqrt

def main():
	f_handle = open("problem42.txt")
	text = f_handle.read()
	f_handle.close()
	words = text.split(',')
	count = 0
#	words = ["SKY"]
	for word in words:
#		print word
		word = sub("[^A-Z]",'',word)
#		print word
		word_num = [ord(c)-64 for c in word]
#		print word
		word_sum = sqrt(2*sum(word_num)+0.25) - 0.5
#		print word_sum
		if word_sum == int(word_sum):
#			print "%s is triangle word" % word
			count += 1
	print count

if __name__ == "__main__":
    main()
