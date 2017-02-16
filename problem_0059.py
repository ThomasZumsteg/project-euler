#!/usr/bin/env python3
"""
Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.
Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.
"""

import itertools
import logging
import string
import sys
import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

FILE = './problem_0059.txt'
WORDS = '/usr/share/dict/words'

with open(WORDS, 'r', encoding='utf-8') as fh:
    dictionary = set(w.strip() for w in fh.readlines())

def score_text(text):
    words_in_dict = 0
    for word_count, word in enumerate(text.split(' ')):
        word = ''.join(c for c in word.lower() if c in string.ascii_lowercase)
        if word in dictionary:
            words_in_dict += 1
    return words_in_dict / word_count

def main():
    with open(FILE, 'r',encoding='utf-8') as fh:
        cipher_text = [int(n) for n in fh.read().split(',')]
    best_guess = None
    for key in itertools.product(string.ascii_lowercase, repeat=3):
        plain_text = []
        for k, c in zip(itertools.cycle(key), cipher_text):
            ch = chr(ord(k) ^ c)
            if ch not in string.printable:
                logging.debug("Not printable: {}".format(''.join(key)))
                continue
            plain_text.append(ch)
        plain_text = ''.join(plain_text)
        try:
            score = score_text(plain_text)
        except ZeroDivisionError:
            score = 0
        logging.debug('{}:{}:{}'.format(''.join(key), score, plain_text))
        best_guess = max(best_guess, (score, ''.join(key), plain_text),
                        key=lambda k: k[0] if k else 0)
    logging.info('Key is {}'.format(''.join(best_guess[1])))
    print('Answer: {}'.format(sum(ord(c) for c in best_guess[2])))

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time()  - start))
