#!/usr/bin/python

from time import time
from common import prime_generator
# BLOCK = 100000

#def main():
#	primes = prime_sieve(BLOCK)
#	i = 0
#	while True:
#		for n_set in num_set_gen(primes):
#			print "%d: %s" %(sum(n_set),str(n_set))
#			if is_prime_set(n_set, primes):
#				print "The set is %s" %str(n_set)
#				print "The sum is %d" %sum(n_set)
#				return None
#
#def is_prime_set(n_set, primes):
#	[a,b] = [len(n_set)-2,len(n_set)-1]
#	done = False
#	while not done:
#		[p,q] = [n_set[a],n_set[b]]
#		pq = int(str(p) + str(q))
#		qp = int(str(q) + str(p))
#		while max(qp,pq) > primes[-1]:
#			print "max(%d, %d) > %d" %( qp,pq,primes[-1])
#			primes.extend(prime_sieve(BLOCK, primes))
#		if qp not in primes or pq not in primes:
#			return False
#		a -= 1
#		if a < 0:
#			b -= 1
#			a = b-1
#			if a < 0: return True
#
#def num_set_gen(primes):
#	set_list = []
#	i = 0
#	set_size = 4
#	next_lowest = 0 
#	while True:
#		if set_list and sum(set_list[0]) < next_lowest:
#			yield set_list[0]
#			set_list.pop(0)
#		elif i + set_size > len(primes):
#			primes.extend(prime_sieve(BLOCK))
#		else:
#			next_lowest = 10 + primes[i+set_size-1]
#			new_sets = get_sets(primes,i, set_size)
#			set_list.extend(new_sets)
#			set_list.sort(key = sum)
#			i += 1
#
#def get_sets(primes, start, num):
#	new_sets = []
#	indexes = [start] * num
#	done = False
#	while not done:
##		print indexes
#		new_set = []
#		for n,i in enumerate(indexes):
#			new_set.append(primes[i+n])
#		new_sets.append(new_set)
#		indexes[0] -= 1
#		for i in range(len(indexes)):
#			if indexes[i] < 0:
#				if i+1 >= len(indexes)-1:
#					done = True
#					break
#				else:
#					indexes[i+1] -= 1
#				indexes[:(i+1)] = [indexes[i+1]]*(i+1)
#	return new_sets

def main():
	set_dict = {}
	limit = False
	max_elements = [1e6] * 5
	i = 0
	for prime in prime_generator(block=1000000):
		i += 1
		if i % 100000 == 0: print "Testing %d" % prime
		if limit and limit < prime: break
		[p,q] = populate(set_dict, prime)
#		print set_dict
#		print "p: %s\nq: %s" %(str(p), str(q))
		element_set = has_valid_set(set_dict,[p,q])
		if element_set and sum(element_set) < sum(max_elements):
			element_set.sort()
			print "%d: %s" %(sum(element_set), str(element_set))
			k = str(element_set[-1])
			limit = int(k + k)
			max_elements = element_set

def populate(set_dict, prime):
	set_dict[prime] = set()
	(p_set, q_set) = (set(), set())
	for i in range(1,len(str(prime))):
		[p,q] = [str(prime)[:i],str(prime)[i:]]
#		print "Adding %s and %s" %(p,q)
		if q[0] == '0':
#			print "Zero dropout: %s" % q
			continue
		elif int(q + p) not in set_dict:
#			print "Not in set_dict: %s" % (q+p)
			continue
		elif int(p) in set_dict and int(q) in set_dict:
			[p,q] = [int(p),int(q)]
			p_set.add(p)
			q_set.add(q)
			set_dict[p].add(q)
			set_dict[q].add(p)
#	if q_set and p_set:
#		print "Adding %d" % prime
#		print "p_set: %s" % str(p_set)
#		print "q_set: %s" % str(q_set)
	return [iter(p_set), iter(q_set)]
		
def has_valid_set(set_dict, canidates):
	c = 0
	elements = []
	while len(elements) < 5:
#		if DEBUG: print "elements: %s" % str(elements)
		try:
#			if DEBUG: print "Trying %d" % c
			canidate = canidates[c].next()
		except IndexError:
#			if DEBUG: print "IndexError caught"
			common_set = set_dict[elements[0]]
			for k in range(1,len(elements)):
				common_set = common_set & set_dict[elements[k]]
			canidates.append(iter(common_set))
		except StopIteration:
#			if DEBUG: print "Iterator %d stopped" % c
			c -= 1
			if c < 0: return False
		else:
#			if DEBUG: 
#				print "Adding set_dict[%d] = %s" % (canidate, len(set_dict[canidate]))
			if set_dict[canidate] >= set(elements):
				elements.append(canidate)
				c += 1
	return elements

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
#	[p_iter, q_iter] = populate(prime_dict, 673109)
#	print has_valid_set(prime_dict,[p_iter,q_iter])
