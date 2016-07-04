#!/usr/bin/python
"""http://projecteuler.net/problem=61"""

from time import time

def main():
	print '-' * 15 + " START " + '-' * 15
	tests = [is_tri, is_square, is_pent, is_hex, is_hep, is_oct]
	cycle = len(tests)
	elements   = [None] * cycle
	generators = [None] * cycle
	i = 0
	while True:
		try: options = generators[i].next()
		except StopIteration:
			i -= 1
		except AttributeError:
			generators[i] = new_gen(range(1000,10000), tests)
		else:
			[elements[i],num_set,tests] = options
#			print "Found element %d for index %d" %(elements[i],i)
			i += 1
			if i < 0: return None
			elif i >= cycle: break
			elif len(tests) <= 1:
				cd = str(elements[0])[:2]
				ab = str(elements[i-1] )[-2:]
				num_set = [int(ab+cd)]
			generators[i] = new_gen(num_set, tests)
	print "%d: %s" %(sum(elements), str(elements))
	
def new_gen(num_set, tests):
#	lead = "\t" * (3-len(tests))
#	if len(num_set) > 1:
#		print lead + "Starting generator %d to %d" %(num_set[0],num_set[-1])
#	else:
#		print lead + "Starting generator %d" %(num_set[0])
#	print lead + "Tests %s" % str([t.__name__ for t in tests])
	for i in num_set:
		if len(str(i)) < 4: continue
		for test in tests:
			if test(i):
#				print lead + "%d is %s" %(i, test.__name__)
				ab = str(i)[-2:]
				new_num_set = range(int(ab+'00'),int(ab+'99')+1)
				new_test = tests[:]
				new_test.remove(test)
				yield [i, new_num_set, new_test]
#	if len(num_set)>1:
#		print lead + "Test range %d to %d done" %( num_set[0],num_set[-1])
#	else:
#		print lead + "Test range %d done" %(num_set[0])

def is_tri(m):
	n  = round((2*m+0.25)**(1.0/2)-0.5)
	nm = n*(n+1)/2
	if nm == m: return True
	else:		return False

def is_square(m):
	n  = round(m**0.5)
	nm = n**2
	if nm == m: return True
	else:	    return False

def is_pent(m):
	n  = round(((2*m+1.0/6)/3.0)**0.5+1.0/6)
	nm = n*(3*n-1)/2
	if nm == m: return True
	else:	    return False

def is_hex(m):
	n  = round((m/2.0+1.0/16)**0.5+0.25)
	nm = n*(2*n-1)
	if nm == m: return True
	else:		return False

def is_hep(m):
	n  = round((2*m/5.0+9.0/100)**0.5+3.0/10)
	nm = n*(5*n-3)/2
	if nm == m: return True
	else:		return False

def is_oct(m):
	n  = round((m/3.0+1.0/9)**0.5+1.0/3)
	nm = n*(3*n-2)
	if nm == m: return True
	else:		return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" % (time() - start)
#	tests = [is_hex, is_hep, is_oct]
#	for i in range(1,66):
#		print "%d is " % i
#		for test in tests:
#			if test(i):
#				print "\t" + test.__name__
