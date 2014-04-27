#!/usr/bin/python
"""http://projecteuler.net/problem=68"""

from time import time
from common import counter

def main():
	n = 5
	max_gon = 0
	for gon in magic_gon_gen(n):
		print gon
		if int(gon) > max_gon and len(gon) == 16:
#		if int(gon) > max_gon:
			max_gon = int(gon)
	print "Maximum magic gon is %s" % max_gon

def magic_gon_gen(n):
	(gon, elem) = make_gon(n)
	init(elem[:n])
	while True:
		for e in elem[n:]:
			e[0] = 0
		if can_set_outside(gon, elem): 
			yield str_gon(gon)
		if not increment_inside(elem[:n]): break

def init(elem):
	for i,e in enumerate(elem):
		e[0] = i + 1

def increment_inside(i_elems):
	l = len(i_elems)
	n = 2 * l
	i = l - 1	# start at the end
	while True:
		e = i_elems[i]
		e[0] += 1
		if i_elems.count(e) == 1 and e[0] <= n:
			if i + 1 == l: break
			else: i += 1
		elif e[0] > n:
			e[0] = i_elems[0][0]
			i -= 1
			if i < 0: return False
#		print i_elems
	return True

def can_set_outside(g,e):
	n = len(e) // 2
	inner_sum = sum([x[0] for x in e[:n]])
	gon_sum = inner_sum / n + (2 * n + 1)
	if inner_sum % n != 0: return False
#	print "e[:n]:%s" %(str(e[:n]))
#	print "n:%d, inner_sum:%d, gon_sum:%d" %(n,inner_sum, gon_sum)
	# determine sum
	for line in g:
		line_sum = sum([x[0] for x in line[1:]])
		if line_sum >= gon_sum: return False
		val = gon_sum - line_sum
		if val > 2 * n: return False
		elif e.count([val]) >= 1: return False
		line[0][0] = val
	return True

def str_gon(g):
	i     = 0
	small = None
	for j,line in enumerate(g):
		if not small or small > line[0][0]:
			small = line[0][0]
			i = j
	g = g[i:] + g[:i]
	g_str = []
	for i in range(len(g)):
#		g_str.append(','.join([str(x[0]) for x in g[i]]))
		g_str.append(''.join([str(x[0]) for x in g[i]]))
#	return ';'.join(g_str)
	return ''.join(g_str)

def make_gon(n):
	elements = [[e] for e in [0] * (n * 2)]
	gon = []
	for i in range(n):
		g_line = [elements[i+n],elements[i],elements[i+1]]
#		print "[elements[%d],elements[%d],elements[%d]]" %(i,i+1,i+n)
		gon.append(g_line)
	gon[-1][2] = elements[0]
	return (gon, elements)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start )
#	(g,e) = make_gon(5)
#	for i,c in enumerate(['a','b','c','d','e','f','g','h','i','j']):
#		e[i][0] = c
#	print e
#	print g
#	g = [[1],[2],[3],[4],[5]]
#	while increment_inside(g):
#		print ','.join([str(x[0]) for x in g])
#	for gon,e in magic_gon_gen(3):
#		print [[gon[j][i][0] for i in range(len(gon[j]))] for j in range(len(gon))]
#		print ""
#		print e
#		print gon
