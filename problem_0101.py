#!/usr/bin/python

from time import time

def main():
	BOP_sum = 0
	f = lambda n: 1 - n + n**2 - n**3 + n**4 - n**5 + n**6 - n**7 + n**8 - n**9 + n**10
	for m in range(1,11):
		matrix = matrix_gen(m)
		for i,line in enumerate(matrix):
			line.append(f(i+1))
		poly = gauss_solver(matrix)
		BOP_sum += sum([p*(m+1)**i for i,p in enumerate(poly)])
	print int(BOP_sum)

def gauss_solver(M):
	for i in range(len(M)):
		col = [line[i] for line in M[i:]]
		big = float(max(col))
		m = col.index(big) + i
		(M[i],M[m]) = (M[m],M[i])
		M[i] = [item/big for item in M[i]]
		for j in range(len(M)):
			if j == i: continue
			val = float(M[j][i])
			for k in range(len(M[j])):
				M[j][k] -= val * M[i][k]
	return [M[i][-1] for i in range(len(M))]

def matrix_gen(size):
	matrix = []
	for i in range(1,size+1):
		line = [i**n for n in range(size)]
		matrix.append(line)
	return matrix

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
