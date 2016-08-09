#!/usr/bin/env python3

import os, re

def rename(old_name):
    match = re.match('problem(\d+)(.*)', old_name)
    if not match:
        return
    num, tail = int(match.group(1)), match.group(2)
    new_name = 'problem_{0:04d}{1}'.format(num, tail)
    # print('{:15} -> {:15}'.format(old_name, new_name))
    os.rename(old_name, new_name)

if __name__ == '__main__':
    for file_name in os.listdir():
        rename(file_name)

