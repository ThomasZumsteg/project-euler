{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub, sort, group)
import Data.List.Split (splitOn)

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

type Hand = (Card, Card, Card, Card, Card)
data Suit = Diamond | Spade | Heart | Club deriving (Show, Eq)
data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight | 
    Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord, Enum)
data Card = Card { suit::Suit, rank::Rank } deriving (Show, Eq)
data HandRank = RoyalFlush | StraightFlush Rank | FourOfAKind Rank Rank | 
    FullHouse Rank Rank | Flush Rank Rank Rank Rank Rank | Straight Rank | 
    ThreeOfAKind Rank | TwoPairs Rank Rank | OnePair Rank | HighCard Rank 
    deriving (Show, Eq, Ord)

problem0054 :: [String] -> [Int]
problem0054 games = error "Not Implemented"

parseHand :: String -> Hand
parseHand str = fromList $ map (\(r:s:[]) -> Card (getSuit s) (getRank r)) cards 
    where
        cards = splitOn " " str

getRank :: Char -> Rank
getRank r = case r of 
    'A' -> Ace
    'K' -> King
    'Q' -> Queen
    'J' -> Jack
    'T' -> Ten
    '9' -> Nine
    '8' -> Eight
    '7' -> Seven
    '6' -> Six
    '5' -> Five
    '4' -> Four
    '3' -> Three
    '2' -> Two
    _ -> error "Not a valid rank"

getSuit :: Char -> Suit
getSuit s = case s of
    'D' -> Diamond
    'S' -> Spade
    'H' -> Heart
    'C' -> Club
    _ -> error "Not a valid suit"

winner :: [Hand] -> Int
winner hands = error "Not Implemented"

rankHand :: Hand -> HandRank
rankHand hand 
    | royalFlush hand = RoyalFlush
    | straightFush hand = StraightFlush (maximum ranks)
    where
        ranks = map rank $ toList hand
        groups = group $ sort ranks

toList :: Hand -> [Card]
toList (c1,c2,c3,c4,c5) = [c1,c2,c3,c4,c5]

fromList :: [Card] -> Hand
fromList (c1:c2:c3:c4:c5:[]) = (c1,c2,c3,c4,c5)

kinds :: Hand -> [(Int, Rank)]
kinds = map (\g -> (length g, head g)) . group . sort . map rank . toList

kindsTest = [
    [(1, Ace), (1, Ten), (3, Nine)] @=? (kinds $ parseHand "AH TS 9D 9S 9C")]

royalFlush :: Hand -> Bool
royalFlush hand = flush hand && royal == (map rank $ toList hand) 
    where
        royal  = [Ace, King, Queen, Jack, Ten]

royalFlushTest = [
    True  @=? (royalFlush $ parseHand "KH QH JH TH AH"),
    False @=? (royalFlush $ parseHand "KH QH JH TH 9H"),
    False @=? (royalFlush $ parseHand "KH QH JH TH AD")]

straightFush :: Hand -> Bool
straightFush hand = straight hand && flush hand

straightFushTest = [
    True  @=? (straightFush $ parseHand "KH QH JH TH 9H"),
    False @=? (straightFush $ parseHand "KH QH JH TH 9D"),
    False @=? (straightFush $ parseHand "KH QH JH TH 8H"),
    True  @=? (straightFush $ parseHand "KH QH JH TH AH"),
    True  @=? (straightFush $ parseHand "2H 4H 3H 5H AH")]

fourOfAKind :: Hand -> Bool
fourOfAKind = (==[4,1]) . map fst . kinds

fourOfAKindTest = [
    True  @=? (fourOfAKind $ parseHand "KD KC KH QC KS"),
    True  @=? (fourOfAKind $ parseHand "KD QD KH KC KS"),
    False @=? (fourOfAKind $ parseHand "KD JD KH QC KS"),
    False @=? (fourOfAKind $ parseHand "AD KD KH QC KS")]

fullHouse :: Hand -> Bool
fullHouse = (==[3,2]) . map fst . kinds

fullHouseTest = [
    True  @=? (fullHouse $ parseHand "KD QD KH QC KS"),
    True  @=? (fullHouse $ parseHand "KD QD QH QC KS"),
    False @=? (fullHouse $ parseHand "KD QD JH QC KS")]


flush :: Hand -> Bool
flush cards = all ((==(suit c)) . suit) cs 
    where
        (c:cs) = toList cards

flushTest = [
    True  @=? (flush $ parseHand "KH QH JH TH 9H"),
    False @=? (flush $ parseHand "KH QH JH TS 9H")]

straight :: Hand -> Bool
straight hand 
    | null sorted = False
    | 1 == length sorted = True
    | otherwise = all (\(c,d) -> c == pred d) $ zip sorted $ tail sorted
    where
        sorted = sort $ map rank $ toList hand

straightTest = [
    True  @=? (straight $ parseHand "KS QD JH TC 9C"),
    False @=? (straight $ parseHand "KS JD QH 9C TC"),
    False @=? (straight $ parseHand "KS QD JH TC 8C"),
    True  @=? (straight $ parseHand "KS QD JH TC AC"),
    False @=? (straight $ parseHand "KS KD TH 2C KC")]

threeOfAKind :: Hand -> Bool
threeOfAKind = (==[3,1,1]) . map fst . kinds 

threeOfAKindTest = [
    True  @=? (threeOfAKind $ parseHand "KS KD TH 2C KC"),
    False @=? (threeOfAKind $ parseHand "KS KD KH 2C KC"),
    False @=? (threeOfAKind $ parseHand "KS QD TH 2C KC"),
    False @=? (threeOfAKind $ parseHand "AS KD TH 2C JC"),
    False @=? (threeOfAKind $ parseHand "QS KD TH 2C KC")]

twoPair :: Hand -> Bool
twoPair = (==[2,2,1]) . map fst . kinds

twoPairTest = [
    True  @=? (twoPair $ parseHand "KS 2D TH 2C KC"),
    False @=? (twoPair $ parseHand "KS JD TH 2C KC"),
    False @=? (twoPair $ parseHand "KS KD 2H 2C KC"),
    False @=? (twoPair $ parseHand "KS KD KH 2C KC")]

onePair :: Hand -> Bool
onePair = (==[2,1,1,1]) . map fst . kinds

onePairTest = [
    True  @=? (onePair $ parseHand "KS JD TH 2C KC"),
    False @=? (onePair $ parseHand "KS JD TH JC KC"),
    True  @=? (onePair $ parseHand "2S JD TH 2C KC"),
    False @=? (onePair $ parseHand "2S 2D TH 2C KC")]

highCard :: Hand -> Bool
highCard _ = True

highCardTest = [
    True  @=? (highCard $ parseHand "KC QS JD 9S 2C"),
    False @=? (highCard $ parseHand "8H 2C 3S 4H KS")]

unitTests = map TestCase $
    kindsTest ++
    royalFlushTest ++
    straightFushTest ++
    fourOfAKindTest ++
    fullHouseTest ++
    flushTest ++
    straightTest ++
    threeOfAKindTest ++
    twoPairTest ++
    onePairTest ++
    highCardTest

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
