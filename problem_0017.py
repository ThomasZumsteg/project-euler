#!/usr/bin/env python3

"""
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage."""

from re import sub
from time import time
from random import randint

def main():
    list_of_nums = []
    for num in range(1,1000):
        string_num = num_to_string(num)
        list_of_nums.append(string_num)
    list_of_nums.append("one thousand")
    letters = ''.join(list_of_nums)
    letters = letters.lower()
    letters = sub("[^a-z]","",letters)
    print(len(letters))

def num_to_string(num):
    str_digits = ["", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine"]
    str_tens   = ["", "", "twenty", "thirty", "forty",
                  "fifty", "sixty", "seventy", "eighty", "ninety"]
    special_cases = {10:"ten", 11:"eleven", 12:"twelve", 13:"thirteen",
                     14:"fourteen", 15:"fifteen", 16:"sixteen",
                     17:"seventeen", 18:"eighteen", 19:"nineteen"}
    string = ""

    if num >= 100:
        string += str_digits[int(str(num)[0])] + " hundred"
        num = num % 100
        if num != 0: string += " and "

    q, r = divmod(num, 10)

    if num == 0:
        pass
    elif num in special_cases:
        string += special_cases[num]
    elif num < 10:
        string += str_digits[num]
    elif r == 0:
        string += str_tens[q]
    else:
        string += str_tens[q] + "-" + str_digits[r]
    return string

if __name__ == "__main__":
    start = time()
    main()
    print('{:0.5f} seconds'.format(time() - start))
