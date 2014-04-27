#!/usr/bin/python

from time import time

def main():
	n_sum = 0
	for n in range(1,101):
		n_sum += sqrt_digit_sum(n)
	print "The sum is %d" %(n_sum)

def sqrt_digit_sum(n):
	digit_sum = 0
	digit = sqrt_digit_gen(n)
	for i in range(100):
		try: digit_sum += digit.next()
		except StopIteration: return 0
	return digit_sum

def sqrt_digit_gen(c):
	while c >= 100:
		c /= 100
	p = 0
	while c > 0:
		for x in reversed(range(0,10)):
			y = (20*p+x)*x
			if y <= c:
				yield x
				c = ( c - y ) * 100
				p = p * 10 + x
				break
	#	print "y: %d, c: %d" %(y,c)
			

if __name__=="__main__":
	start = time()
	main()
	print "That took %f seconds" %(time()-start)
