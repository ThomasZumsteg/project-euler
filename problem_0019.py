#!/usr/bin/python

def main():
	start = False
	count = 0
	for date in date_generator():
		if date[:-1] == (1,1,1901): 
			print(date)
			start = True
		if start:
			if date[-1] == 6 and date[0] == 1:
				count += 1
				print(date)
#		if date[:-2] == (1,1): print "%r: %d: %d" %(start, count, date[2]) 
		if date[:-1] == (31,12,2000): 
			print(date)
			break
	print(count)

def date_generator():
	count = 0
	thirty_day = lambda x : 30
	thirty_one  = lambda x : 31
	year = 1900
	days_in_month = (thirty_one, february,   thirty_one, thirty_day,
					 thirty_one, thirty_day, thirty_one, thirty_one,
					 thirty_day, thirty_one, thirty_day, thirty_one)
	while True:
		for month in range(1,13):
			days = days_in_month[month-1](year)
			for day in range(1,days+1):
				date = (day, month, year, count%7)
				yield date
				count += 1
		year += 1

def february(year):
	if year % 4 == 0:
		if year % 100 == 0:
			if year % 400 == 0: return 29
			return 28
		return 29
	return 28

if __name__=="__main__":
	days = ["Monday", "Tuesday", "Wednesday", "Thursday",
			"Friday", "Saturday", "Sunday"]
	main()
#	for date in date_generator():
#		if date[:-1] == (31,12,2000):
#			print str(date[:-1]) + ":" +  str(days[date[-1]])
#			break
