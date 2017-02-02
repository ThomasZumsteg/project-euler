{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub, sort, sortBy, group)
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

data Hand = Hand Card Card Card Card Card deriving (Show)
data Suit = Diamond | Spade | Heart | Club deriving (Show, Eq)
data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight | 
    Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord, Enum)
data Card = Card { rank::Rank, suit::Suit } deriving (Show, Eq)
data HandRank = RoyalFlush | StraightFlush | FourOfAKind | FullHouse | 
                Flush | Straight | ThreeOfAKind | TwoPair | OnePair | HighCard 
                deriving (Show, Eq, Ord)

instance Eq Hand where
    h1 == h2 = kinds h1 == kinds h2
    
instance Ord Hand where
    h1 `compare` h2 
        | h1 /= h2 = compare (rankHand h1) (rankHand h2) 
        | otherwise = compare kinds_1 kinds_2
        where
            kinds_1 = map snd $ kinds h1
            kinds_2 = map snd $ kinds h2

problem0054 :: [String] -> [(Ordering, String, String)]
problem0054 [] = []
problem0054 (g:games) = (winner, c1, c2):problem0054 games
    where
        c1 = take 14 g
        c2 = take 14 $ drop 15 g
        winner = compare (parseHand c1) (parseHand c2)

parseHand :: String -> Hand
parseHand str = fromList $ map (\(r:s:[]) -> Card (getRank r) (getSuit s)) cards 
    where
        cards = splitOn " " str

parseHandTest = [
    Hand (Card Two Spade) (Card Jack Diamond) (Card Ten Heart) (Card Two Club) 
        (Card King Club) @=? parseHand "2S JD TH 2C KC"]

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

rankHand :: Hand -> HandRank
rankHand hand 
    | royalFlush hand = RoyalFlush
    | straightFush hand = StraightFlush 
    | fourOfAKind hand = FourOfAKind
    | fullHouse hand = FullHouse
    | flush hand = Flush
    | straight hand = Straight
    | threeOfAKind hand = ThreeOfAKind
    | twoPair hand = TwoPair
    | onePair hand = OnePair
    | highCard hand = HighCard
    | otherwise = error "Not a rankable hand"

toList :: Hand -> [Card]
toList (Hand c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]

fromList :: [Card] -> Hand
fromList (c1:c2:c3:c4:c5:[]) = Hand c1 c2 c3 c4 c5

kinds :: Hand -> [(Int, Rank)]
kinds = sortBy numAndRank . map groupSize . group . sort . map rank . toList
    where
        numAndRank (a_count, a_rank) (b_count, b_rank)
            | a_count == b_count = compare a_rank b_rank
            | otherwise = compare b_count a_count
        groupSize g = (length g, head g)


kindsTest = [
    [(3, Nine), (1, Ace), (1, Ten)] @=? (kinds $ parseHand "AH TS 9D 9S 9C")]

royalFlush :: Hand -> Bool
royalFlush hand = flush hand && royal == (map snd $ kinds hand) 
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
    | sorted == [Ace, Five, Four, Three, Two] = True
    | otherwise = all (\(c,d) -> c == pred d) $ zip sorted $ tail sorted
    where
        sorted = sort $ map rank $ toList hand

straightTest = [
    -- False @=? (straight $ parseHand "5C AD 5D AC 9C"),
    False @=? (straight $ parseHand "7C 5H 8D TD KS"),
    True  @=? (straight $ parseHand "KS QD JH TC 9C"),
    True  @=? (straight $ parseHand "KS JD QH 9C TC"),
    False @=? (straight $ parseHand "KS QD JH TC 8C"),
    True  @=? (straight $ parseHand "KS QD JH TC AC"),
    True  @=? (straight $ parseHand "5S 4D 3H 2C AC"),
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
    True  @=? (highCard $ parseHand "8H 2C 3S 4H KS")]

unitTests = map TestCase $
    parseHandTest ++
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
    text <- readFile "problem_0054.txt"
    let  answer = problem0054 $ lines text
    printf "Answer: %d\n" (length $ filter (\(o,_,_) -> o == GT) answer)
exec AdHoc = do
    text <- readFile "problem_0054.txt"
    let  answer = problem0054 $ lines text
    let win o = if o == GT then "winner" else ""
    mapM_ (\(o, c1, c2) -> printf "%s - %s - %s\n" c1 c2 (win o)) answer
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
