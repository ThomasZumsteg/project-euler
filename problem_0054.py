#!/usr/bin/env python3
"""
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:
    Player 1: 5H 5C 6S 7S KD (Pair of Fives)
    Player 2: 2C 3S 8S 8D TD (Pair of Eights)
    Winner  : Player 2

    Player 1: 5D 8C 9S JS AC (Highest card Ace)
    Player 2: 2C 5C 7D 8S QH (Highest card Queen)
    Winner  : Player 1

    Player 1: 2D 9C AS AH AC (Three Aces)
    Player 2: 3D 6D 7D TD QD (Flush with Diamonds)
    Winner  : Player 2

    Player 1: 4D 6S 9H QH QC (Pair of Queens, Highest card Nine)
    Player 2: 3D 6D 7H QD QS (Pair of Queens, Highest card Seven)
    Winner  : Player 1

    Player 1: 2H 2D 4C 4D 4S (Full House, With Three Fours)
    Player 2: 3C 3D 3S 9S 9D (Full House, with Three Threes)
    Winner  : Player 1

The file, problem_0054.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
"""

from time import time
from re import match,search,sub
import logging
import sys

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    wins = 0
    for line in open('problem_0054.txt'):
        [player1, player2] = parse_line(line)
        if one_wins(player1, player2): wins += 1
    print('Answer: {}'.format(wins))

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
    logging.debug("P1: {} P2: {}".format(p1_rank, p2_rank))
    if p1_num < p2_num :
        logging.debug("P1 Wins")
        return True
    elif p1_num > p2_num: return False
    return high_card(p1,p2)

def high_card(p1, p2):
    value = '23456789TJQKA'
    for i in reversed(range(5)):
        logging.debug("Comparing p1: {} and p2: {}".format(p1[i*3], p2[i*3]))
        if value.index(p1[i*3]) > value.index(p2[i*3]):
            logging.debug("P1 Wins")
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
    logging.info("That took {:4.2f} seconds".format(time() - start))
