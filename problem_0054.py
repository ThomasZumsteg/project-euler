#!/usr/bin/python
"""http://projecteuler.net/problem=54"""

from time import time
from re import match,search,sub

def main():
	wins = 0
	for line in open('problem54.txt'):
		[player1, player2] = parse_line(line)
		if one_wins(player1, player2): wins += 1
	print wins

def parse_line(line):
	value = '23456789TJQKA'
	cards = line.split()
	players = [cards[:5], cards[5:]]
	for n in [0,1]:
		players[n].sort(key=lambda card: [value.index(c) for c in card[0]])
		players[n] = ' '.join(players[n])
	return players

def one_wins(p1, p2):
	(p1, p1_num, p1_rank) = rank_hand(p1)
	(p2, p2_num, p2_rank) = rank_hand(p2)
#	print "P1: %s P2: %s" %(p1_rank, p2_rank)
	if p1_num < p2_num :
#		print "P1 Wins"
		return True
	elif p1_num > p2_num: return False
	return high_card(p1,p2)

def high_card(p1, p2):
	value = '23456789TJQKA'
	for i in reversed(range(5)):
#		print "Comparing p1: %s and p2: %s" %(p1[i*3], p2[i*3])
		if value.index(p1[i*3]) > value.index(p2[i*3]):
#			print "P1 Wins"
			return True
		elif value.index(p1[i*3]) < value.index(p2[i*3]): return False

def rank_hand(hand):
	value = '23456789TJQKA'
	ranks = [lambda hand: royal_flush(hand),
			 lambda hand: straight(hand) and flush(hand),
			 lambda hand: four_kind(hand),
			 lambda hand: full_house(hand),
			 lambda hand: flush(hand),
			 lambda hand: straight(hand),
			 lambda hand: three_kind(hand),
			 lambda hand: two_pair(hand),
			 lambda hand: one_pair(hand),
			 lambda hand: hand]
	hands = ["Royal Flush",
			 "Straight Flush",
			 "Four of a Kind",
			 "Full House",
			 "Flush",
			 "Straight",
			 "Three of a Kind",
			 "Two Pair",
			 "One Pair",
			 "High Card " + hand ]
	for i, rank in enumerate(ranks):
		ranked_hand = rank(hand)
		if ranked_hand:
			return (ranked_hand, i, hands[i])

def one_pair(hand):
	match = search(r" ?(.). \1.", hand)
	if match:
		hand = sub(match.group(), '', hand)
		hand = hand.strip()
		if not match.group()[0] == ' ': hand += ' '
		return hand + match.group()
	return False

def two_pair(hand):
	if match(r".. (.). \1. (.). \2.", hand): return hand
	elif match(r"(.). \1. (.). \2. ..", hand):
		return hand[-2:] + ' ' + hand[:-3]
	elif match(r"(.). \1. .. (.). \2.", hand):
		return hand[6:9] + hand[:6] + hand[-5:]
	return False

def four_kind(hand):
	match = search(r"(.).( \1.){3} ?", hand)
	if match:
		hand = sub(match.group(), '', hand)
		hand = hand.strip()
		if not match.group()[0] == ' ': hand += ' '
		return hand + match.group()
	return False

def three_kind(hand):
	match = search(r"(.). \1. \1. ?", hand)
	if match:
		hand = sub(match.group(), '', hand)
		hand = hand.strip()
		if not match.group()[0] == ' ': hand += ' '
		return hand + match.group()
	return False

def royal_flush(hand):
	match = search(r"T(.) J\1 Q\1 K\1 A\1", hand)
	if match:
		hand = sub(match.group(), '', hand)
		hand = hand.strip()
		if not match.group()[0] == ' ': hand += ' '
		return hand + match.group()
	return False

def straight(hand):
	value = '23456789TJQKA'
	# 			2C 3H 4S 5C 6D
	hvalues = ''
	for i in range(5):
		hvalues += hand[i*3]
	if search(hvalues, value): return hand
	else: return False

def flush(hand):
	if match(r".(.)( .\1){4}", hand): return hand

def full_house(hand):
	if match(r"(.). \1. \1. (.). \2.", hand):
		return hand[6:] + ' ' + hand[:5]
	elif match(r"(.). \1. (.). \2. \2.", hand): return hand
	else: return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
#	print rank_hand("5H 5C 6S 7S KD")
