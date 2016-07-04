#!/usr/bin/python
"""http://projecteuler.net/problem=70"""

from time import time
from common import prime_sieve

def main():
	phi_low = ()
	limit = 10**7
	for n, f in factor_gen(limit):
		n_phi = phi(f, n)
		if is_perm(n,n_phi):
#			print "n:%d, phi(n):%d" %(n,n_phi)
			phi_fract = float(n)/n_phi
			if not phi_low or phi_low[2] > phi_fract:
				phi_low = (n,n_phi,phi_fract)
	print "Lowest n:%d, phi(n):%d, n/phi(n):%f" %phi_low

def factor_gen(limit):
	limit -= 1
	numbers = [ [] for x in range(limit) ]
	offset = 2
	for i, n in enumerate(numbers):
		if not n:
			for j in range(i,limit,i+offset):
				numbers[j].append(i+offset)
		yield (i+offset, n)

def is_perm(n,m):
	n = sorted([int(x) for x in list(str(n))])
	m = sorted([int(x) for x in list(str(m))])
	if n == m: return True
	else:      return False

def phi(fact, val):
	p = val
	for f in fact:
		p *= 1-1.0/f
	return int(p)

#def phi_gen(num):
#	primes = prime_sieve(num-2)
#	primes.reverse()
#	indexes = [0]
#	powers  = [1]
##	print primes
#	while True:
#		factors = [primes[i] for i in indexes]
#		val = evaluate(indexes, powers, primes)
#		p = phi(factors, val)
#		print "val: %d, phi: %d, val/phi: %f" %(val,p,val/float(p))
#		yield (p, val)
#		if increase_powers(indexes,powers,primes,num):
#			pass
#		elif new_factor_set(indexes,powers,primes,num):
#			pass
#		else:
#			break

#def increase_powers(indexes,powers,primes,num):
#	for i in reversed(range(len(indexes))):
#		val = evaluate(indexes,powers,primes)
#		if val*primes[indexes[i]] <= num:
#			powers[i] += 1
#			return True
#		else:
#			powers[i] = 1
#	return False
#
#def new_factor_set(indexes,powers,primes,num):
#	i = indexes[-1] - 1
#	val = evaluate(indexes,powers,primes)
#	if val*primes[i] < num and i >= 0:
#		while val*primes[i] < num and i > 0:
#			i -= 1
#		indexes.append(i+1)
#		powers.append(1)
#		return True
#	for i in reversed(range(1,len(indexes))):
#		if indexes[i-1] > indexes[i]+1:
#			indexes[i] += 1
#			return True
#		else:
#			indexes.pop()
#	if indexes[0] < len(primes)-1:
#		indexes[0] += 1
#		return True
#	else:
#		return False
#
#def evaluate(indexes,power,primes):
#	val = 1
#	factors = [primes[i] for i in indexes]
#	for f,p in zip(factors,power):
#		val *= f**p
#	return val

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
