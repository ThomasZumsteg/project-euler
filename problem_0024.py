#!/usr/bin/python
"""A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

	012   021   102   120   201   210

	What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"""

from re import sub

def main():
	count = 1
	for val in lexicographic_gen('0123456789'):
		if count == 1e6:
			print val
			break
		count += 1

def lexicographic_gen(o_list):
	done = False
	indexes = [0] * len(o_list)
	while not done:
#		print indexes
		temp = o_list
		string = ''
		for i, index in enumerate(indexes):
			string += temp[index]
			temp = temp[:index] + temp[index+1:]
		yield string
		indexes[-1] += 1
		(done, indexes) = reorder(indexes)

def reorder(indexes):
	i_lim = range(len(indexes))
	done = False
	swap_next = False
	for lim, i, val in zip(i_lim,reversed(i_lim),reversed(indexes)):
#		print "index:%d val:%d limit:%d" %(i,val,lim) 
		if swap_next:
			indexes[i] += 1
			val += 1
		swap_next = False
		if val > lim:
#			print "swapped"
			indexes[i] = 0
			swap_next = True 
	return (swap_next, indexes)

if __name__ == "__main__":
	main()
#	(done, indexes) = reorder([2,1,1])
#	print str(done) + " + " + str(indexes)
