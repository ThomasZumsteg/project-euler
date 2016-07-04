#!/usr/bin/python
"""Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 x 53 = 49714.

What is the total of all the name scores in the file?"""

from re import sub

def main():
	f = open("problem22_names.txt")
	line = f.read()
	f.close()
	names = line.split(",")
	names = [sub("[^A-Z]","",x) for x in names]
	names.sort()
	name_sum = 0
	for i, name in enumerate(names):
		name_val = name_to_val(name)
		name_sum += name_val * (i+1)
	print name_sum

def name_to_val(name):
	upper_ascii = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	name_val = 0
	for i,c in enumerate(upper_ascii):
		letter_count = name.count(c)
		name_val += (i+1)*letter_count
	return name_val


if __name__ == "__main__":
	main()
