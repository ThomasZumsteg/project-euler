{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sort, group)

-- In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

--     High Card: Highest value card.
--     One Pair: Two cards of the same value.
--     Two Pairs: Two different pairs.
--     Three of a Kind: Three cards of the same value.
--     Straight: All cards are consecutive values.
--     Flush: All cards of the same suit.
--     Full House: Three of a kind and a pair.
--     Four of a Kind: Four cards of the same value.
--     Straight Flush: All cards are consecutive values of same suit.
--     Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

-- The cards are valued in the order:
-- 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

-- If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

-- Consider the following five hands dealt to two players:
--     Player 1: 5H 5C 6S 7S KD (Pair of Fives)
--     Player 2: 2C 3S 8S 8D TD (Pair of Eights)
--     Winner  : Player 2

--     Player 1: 5D 8C 9S JS AC (Highest card Ace)
--     Player 2: 2C 5C 7D 8S QH (Highest card Queen)
--     Winner  : Player 1

--     Player 1: 2D 9C AS AH AC (Three Aces)
--     Player 2: 3D 6D 7D TD QD (Flush with Diamonds)
--     Winner  : Player 2

--     Player 1: 4D 6S 9H QH QC (Pair of Queens, Highest card Nine)
--     Player 2: 3D 6D 7H QD QS (Pair of Queens, Highest card Seven)
--     Winner  : Player 1

--     Player 1: 2H 2D 4C 4D 4S (Full House, With Three Fours)
--     Player 2: 3C 3D 3S 9S 9D (Full House, with Three Threes)
--     Winner  : Player 1

-- The file, problem_0054.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

-- How many hands does Player 1 win?

data EulerArgs =
    Euler
    | AdHoc
    | UnitTest
    deriving (Show, Data, Typeable)

type Hand = [Card]
data Suit = Diamond | Spade | Heart | Club deriving (Show, Eq)
data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight | 
    Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord)
data Card = Card { suit::Suit, rank::Rank } deriving (Show, Eq)
data HandRank = RoyalFlush | StraightFlush Rank | FourOfAKind Rank Rank | 
    FullHouse Rank Rank | Flush Rank Rank Rank Rank Rank | Straight Rank | 
    ThreeOfAKind Rank Rank Rank | TwoPairs Rank Rank Rank | 
    OnePair Rank Rank Rank Rank | HighCard Rank Rank Rank Rank Rank
    deriving (Show, Eq, Ord)

problem0054 :: [String] -> [Int]
problem0054 games = map (winner . parseHands) games

parseHands :: String -> [Hand]
parseHands = error "Not Implemented"

winner :: [Hand] -> Int
winner hands = error "Not Implemented"

royalFlush :: Hand -> Bool
royalFlush hand = flush hand && royal == (map rank hand) 
    where
        royal  = [Ace, King, Queen, Jack, Ten]

straightFush :: Hand -> Bool
straightFush hand = straight hand && flush hand

fourOfAKind :: Hand -> Bool
fourOfAKind = error "Not Implemented"

fullHouse :: Hand -> Bool
fullHouse = error "Not Implemeneted"

flush :: Hand -> Bool
flush [] = False
flush (c:cards) = all ((==(suit c)) . suit) cards

flushTest = [
    False @=? flush ((Card Spade Two): [Card Heart r | r <- [Ace, Ten, Nine, Six]]),
    True @=? flush [Card Heart r | r <- [Ace, Ten, Nine, Six, Two]],
    False @=? flush []]

straight :: Hand -> Bool
straight = error "Not Implemented"

threeOfAKind :: Hand -> Bool
threeOfAKind = error "Not Implemented"

twoPair :: Hand -> Bool
twoPair = error "Not Implemented"

onePair :: Hand -> Bool
onePair = error "Not Implemented"

highCard :: Hand -> Bool
highCard = error "Not Implemented"

unitTests = map TestCase $
    flushTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0054 []
    printf "Answer: %d\n" (length $ filter (==1) answer)
exec AdHoc = do
    let answer = problem0054 []
    return ()
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
