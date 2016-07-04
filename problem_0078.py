#!/usr/bin/python
"""http://projecteuler.net/problem=78"""

from time import time
from progressbar import ProgressBar

def main():
	for n, pn in euler_gen():
		if pn % 1000000 == 0:
			print "%d : %d" %(n,pn)
			break

def euler_gen():
	Pn = [1]
	n = 0
	nk = [1,2]
	ik = -1
	fk = lambda x: x*(3*x-1)/2
	while True:
		n += 1
		if n == nk[-1]:
			ik = -ik
			if ik > 0: ik += 1
			nk.append(fk(ik))
#			print "Now %s" % str(nk)
		Pn.append(0)
#		print "%d = " %(n),
		for i, k  in enumerate(nk[:-1]):
			Pn[-1] += Pn[n-k] * (-1)**(i//2)
#			print "+ Pk[%d-%d]:%d" %(n,k,Pn[n-k]),
#		print ""
		yield n,Pn[-1]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
