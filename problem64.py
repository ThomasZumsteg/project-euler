#!/usr/bin/python
"""http://projecteuler.net/problem=64"""

from time import time

def main():
	odd = 0
	for num in range(10000):
		period = find_period(num)
		if period % 2 == 1: odd += 1
	ps="There are %d coninued fractions with odd periods below 10000"
	print ps % odd

def find_period(n):
	remainers = []
	digits = []
	for a,rem in const_fract_gen(n):
		try: count = remainers.index(rem)
		except ValueError:
			remainers.append(rem)
			digits.append(a)
		else: 
			count = len(remainers) - count
			break
	digits = ''.join([str(n) for n in digits])
	try: count
	except NameError:
#		print "%s" % digits
		return 0
	else:
#		print "[%s;(%s)]" % (str(digits[:-count]),str(digits[-count:]))
		return count

def const_fract_gen(n):
	m = 0
	d = 1
	a = a0 = int(n**0.5)
	while True:
#		print "\n%d + (sqrt(%d)-%d)/%d" %( a, n, d*a-m, d)
		yield (a,(d,m))
		m = d * a - m
		d = (n - m**2) / d
		try: a = (a0 + m) // d
		except ZeroDivisionError: break

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
