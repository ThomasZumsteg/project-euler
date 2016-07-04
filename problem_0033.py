#!/usr/bin/python
"""http://projecteuler.net/problem=33"""

def main():
	numerator   = 1
	denominator = 1
	for numer in range(10,99):
		for denom in range(numer+1,100):
			for n,d in remove_duplicate_digits(numer, denom):
				if n * denom == d * numer:
					print "%d/%d = %d/%d" %(n,d,numer,denom)
					numerator   *= n
					denominator *= d
	print "%d/%d" %(numerator, denominator)

def remove_duplicate_digits(a,b):
	[a,b] = [str(a), str(b)]
	for i in range(len(a)):
		if a[i] == '0': continue
		for j in range(len(b)):
			if a[i] == b[j]:
				dup_a = int(a[:i] + a[i+1:])
				dup_b = int(b[:j] + b[j+1:])
				if not (dup_a == 0 or dup_b == 0):
					yield [dup_a, dup_b]


if __name__ == "__main__":
	main()
