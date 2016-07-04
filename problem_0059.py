#!/usr/bin/python
"""http://projecteuler.net/problem=59"""

from time import time
from re import sub

def main():
	crypt_file = open("problem59.txt").read()
	crypt_file = crypt_file.split(',')
	crypt_file = [int(x) for x in crypt_file]
	key = ''
#	alpha = 'abcdefghijklmnopqrstuvwxyz'
	for i in range(3):
		crypt_set = crypt_file[i::3]
		key += find_key(crypt_set)
	print sum(decrypt(crypt_file, key))

def decrypt(crypt, key):
	key_len = len(key)
	ascii_nums = []
	string = ''
	for i,n in enumerate(crypt):
		num = n ^ ord(key[i % key_len])
		string += chr(num)
		ascii_nums.append(num)
	print string
	return ascii_nums

def find_key(crypt_set):
#	things = ' etaoi'
	freq_characters = ' '
	most_likely = ''
	for c in freq_characters:
		decrypt = [chr(n ^ ord(c)) for n in crypt_set]
		decrypt = ''.join(decrypt)
		decrypt = sub(r"[^A-Za-z]",'',decrypt)
		decrypt = list(decrypt.lower())
#		print decrypt
		most_likely += max(set(decrypt), key=decrypt.count)
#		if search(r"[a-zA-Z]", ''.join(decrypt)): return c
	print "Probably one of [%s]" % most_likely
	most_likely = list(most_likely)
	most_likely = max(set(most_likely), key=most_likely.count)
	return most_likely

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time()  - start)
#	thing = [79,59,12,2,79,35,8,28,20,2,3,68,8,9,68,45,0,12,9,67,68,4,7,5,23,27,1,21,79,85,78,79,85,71,38,10,71,27,12,2,79,6,2,8,13,9,1,13,9,8,68,19,7,1,71,56,11,21,11,68,6,3,22,2,14,0,30,79,1,31,6,23,19,10,0,73,79,44,2,79,19,6,28,68,16,6,16,15,79,35,8,11,72,71,14,10,3,79]
#	find_key(thing[2::3],'abcdefghijklmnopqrstuvwxyz')
	# god
