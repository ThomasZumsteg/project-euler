#!/usr/bin/python
"""http://projecteuler.net/problem=45"""

from common import counter

def main():
	p_gen = pent_gen()
	t_gen = tri_gen()
	pent_list = [p_gen.next()]
	tri_list  = [t_gen.next()]
	first = True
	for n in counter(143):
		hex_num = n*(2*n-1)
		while hex_num > pent_list[-1]:
			pent_list.append(p_gen.next())
		while hex_num > tri_list[-1]:
			tri_list.append(t_gen.next())
		if hex_num in pent_list and hex_num in tri_list:
			print hex_num
			if not first: break
			first = False

def pent_gen():
	for n in counter(1):
		yield n*(3*n-1)/2

def tri_gen():
	for n in counter(1):
		yield n*(n+1)/2

if __name__ == "__main__":
	main()
