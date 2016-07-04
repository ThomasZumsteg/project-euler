#!/usr/bin/python
"""http://projecteuler.net/problem=104"""

from time import time
from sys import stdout

def main():
	for n,f in enumerate(fib_gen()):
#		print "%d: %d\r" %(n,f)
		stdout.write("%d: %d\r" %(n,len(str(f))))
		stdout.flush()
		if has_pandigital_ends(f):
			print "%d has pandigitial ends" %(n)
			print "%s ... %s" %(str(f)[:9],str(f)[-9:])
			return

def has_pandigital_ends(num):
	s_num = str(num)
	front = set(s_num[  :9])
	back  = set(s_num[-9: ])
	if front != back:
		return False
	elif '0' in back:
		return False
	elif len(back) < 9:
		return False
	else:
		return True 

def fib_gen():
	a = 1
	b = 0
	yield b
	while True:
		yield a
		(a,b) = (a+b,a)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start )
