def even_fib_sum(limit):
	a = 1
	b = 2
	the_sum = 0
	while b < limit:
		if b % 2 == 0:
			the_sum += b
		a, b = b, a+b
	return the_sum

if __name__ == "__main__":
	print(even_fib_sum(4e6))
