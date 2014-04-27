#!/usr/bin/python

def main():
	for a_set in sum_n_equal_m(3,1000):
		[a,b,c] = a_set
		if a**2 + b**2 == c**2:
			return a*b*c

def sum_n_equal_m(n,m):
	num_set = range(1,n+1)
	num_set[-1] = m-sum(num_set[:-1])
	while True:
		yield num_set
		num_set[-1] -= 1
		num_set[-2] += 1
		if not ordered(num_set):
			num_set = reorder(num_set)
			if not num_set:
				break 

def ordered(a_list):
	for m,n in zip(a_list[:-1],a_list[1:]):
		if m >= n:
			return False
	return True

def reorder(a_list):
	list_sum = sum(a_list)
	while not ordered(a_list):
		index = None
		for i in range(len(a_list)-1):
			if a_list[i] >= a_list[-1]:
				index = i
				break
		if index <= 0:
			return False
		a_list[index-1] += 1
		for j in range(index, len(a_list)-1):
			a_list[j] = a_list[j-1] + 1
		a_list[-1] = list_sum - sum(a_list[:-1])
	return a_list

if __name__ == "__main__":
	print main()
