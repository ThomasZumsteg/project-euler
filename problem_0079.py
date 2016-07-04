#!/usr/bin/python
"""http://projecteuler.net/problem=79"""

from time import time
from re import sub,	compile, search

def main():
	regexs = []
	find = r'(\d)(\d)(\d)'
	replace = r'\1.*\2.*\3'
	for line in open("problem79.txt"):
		rex = compile(sub(find, replace, line[:-1]))
		regexs.append(rex)
	sn = str(0)
	done = False
	while not done:
		sn = str(int(sn)+1)
#		print sn
		done = True
		for rex in regexs:
			if not rex.search(sn):
				done = False
				break
	print "The key is %s" % sn

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
