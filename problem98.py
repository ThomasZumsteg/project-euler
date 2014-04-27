#!/usr/bin/python
"""http://projecteuler.net/problem=98"""

from time import time
from itertools import count, combinations
from progressbar import ProgressBar

def main():
	p = ProgressBar()
	words   = find_anagrams(word_gen("problem98.txt"))
	size = max([len(w) for w in words.keys()])
	squares = find_anagrams(square_gen(10**(size+1)))
	biggest = 0
	for word_list in words.values():
		for num_list in squares.values():
			for m in anagrams(word_list, num_list):
				biggest = max(m,biggest)
	print "Largest Anagramic square is %d" %(biggest)
	
def anagrams(words, nums):
	for a_word, b_word in combinations(words,2):
		for a_num, b_num in combinations(nums,2):
			if maps(a_word, str(a_num), b_word,str(b_num)):
				print "(%d:%d) (%s:%s)"%(a_num,b_num,a_word,b_word)
				yield max(a_num, b_num)

def maps(A,a,B,b):
	char_map = {}
	if len(A) != len(a): return False
	for C,c in zip(A,a):
		char_map[c] = C
#	print char_map
	for i,char in enumerate(b):
#		print "Comparing %s: %s" %(char_map[char], b[i])
		if B[i] != char_map[char]: return False
	return True

def word_gen(file_name):
	line = open(file_name).read()
	line = line.replace('"','').strip()
	for word in line.split(','):
		yield word

def square_gen(max_size):
	for N in range(1,int(max_size**0.5)):
		yield N**2

def find_anagrams(func):
	anagrams = {}
	for word in func:
		letters = ''.join(sorted(str(word)))
		try: anagrams[letters].append(word)
		except KeyError:
			anagrams[letters] = [word]
	for k in anagrams.keys():
		if len(anagrams[k]) <= 1:
			anagrams.pop(k,None)
	return anagrams

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
