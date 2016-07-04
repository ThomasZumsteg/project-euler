#!/usr/bin/python
"""http://projecteuler.net/problem=102"""

from time import time

def main():
	has_origin = 0
	f_name = "problem102.txt"
	triangles = read_file(f_name)
	for t in triangles:
		if contains_point(t,(0,0)):
			has_origin += 1
#			print "True : %s" %(str(t))
#		else:
#			print "False: %s" %(str(t))
	print "%d trianges contain the origin" %(has_origin)

def contains_point(t,p):
	b1 = sign(t[0],t[1],p)
	b2 = sign(t[1],t[2],p)
	b3 = sign(t[2],t[0],p)
	if b1 == 0 or b2 ==0 or b3 == 0:
		print t
	return ((b1<0)==(b2<0)) and ((b2<0)==(b3<0))

def sign(p1,p2,p3):
	(x,y) = (0,1)
	n1 = (p2[x]-p1[x]) * (p3[y]-p1[y])
	n2 = (p3[x]-p1[x]) * (p2[y]-p1[y]) 
	return n1 - n2

def read_file(name):
	triangles = []
	for line in open(name):
		tri = [int(n) for n in line.split(',')]
		tri = [(tri[2*i],tri[2*i+1]) for i in range(3)]
		triangles.append(tri)
	return triangles

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
