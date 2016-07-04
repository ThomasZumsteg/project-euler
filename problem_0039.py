#!/usr/bin/python
"""http://projecteuler.net/problem=39"""

def main():
	max_int_tri = (0,0)
	for i in range(5,1001):
		int_count = int_right_tri(i)
		if max_int_tri[0] < int_count:
			max_int_tri = (int_count, i)
			print max_int_tri
	print max_int_tri

def int_right_tri(perim):
	count = 0
	for a in range(1,int(perim/3)+1):
		b = a
		while b < perim-(a+b):
			c = perim-(a+b)
#			print "%d %d %d" %(a,b,c)
			if a**2 + b**2 == c**2:
				count += 1
#				print "{%d, %d, %d}" %(a,b,c)
			b+=1
	return count

if __name__ == "__main__":
    main()
