module Plays (
  -- | Sets of cards that can be played.  Do not construct Plays manually; use
  -- allPlays and filter.
  Play(..),
  Plays(..),
  allPlays,

  -- | Utility functions for interacting with Plays
  playLength,
  unwrapPlay,
  isBomb,
  ) where

import Cards

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List as List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

-- Note that the utility functions below assume these are legitimate plays that
-- are internally consistent.  The results are undefined if you create these
-- objects manually.

-- | Type alias representing a set of possible plays.
type Plays = Set Play

-- | Represents a Tichu play.  (Not to be confused with Google Play.)
data Play =
  Single Card -- ^ Play a single card
  | Multiple Cards -- ^ Play 2, 3, or 4 cards
  | Straight Cards -- ^ Play a 5-14 card straight
  | FullHouse Cards Cards -- ^ Play a full house (high three, low two)
  | ConsPairs [Cards] -- ^ Play consecutive pais (low to high)
  | Wish Play Rank -- ^ Make a play that includes a wish
  deriving (Ord, Eq)

-- | Extract the set of cards a play uses.
unwrapPlay :: Play -> Cards
unwrapPlay (Single x) = Set.singleton x
unwrapPlay (Multiple xs) = xs
unwrapPlay (Straight xs) = xs
unwrapPlay (FullHouse xs ys) = xs `Set.union` ys
unwrapPlay (ConsPairs xs) = Set.unions xs
unwrapPlay (Wish x _) = unwrapPlay x

-- | Plays have ranks; the rank is the lowest card of a straight or cons pair,
-- the value of a single, pair, triple, or four-card bomb,
instance Ranked Play where
  rank (Single Phoenix) = 1 -- TODO(jrockway): Fix after #1 is resolved.
  rank (Single x) = rank x
  rank (Multiple xs) = rank . arbitraryStandardCard $ xs
  rank (Straight xs) = if Set.member Phoenix xs && min + len - 1 > 14 then
                         Rank (min - 1) else Rank min
    where min = fromIntegral . rank . Set.findMin $ xs
          len = Set.size xs
  rank (FullHouse highs lows) = rank . arbitraryStandardCard $ highs
  rank (ConsPairs (x:xs)) = rank . arbitraryStandardCard $ x

-- | Return the length of a play.
playLength :: Play -> Int
playLength (Single _) = 1
playLength (Multiple xs) = Set.size xs
playLength (Straight xs) = Set.size xs
playLength (FullHouse _ _) = 5
playLength (ConsPairs xs) = length xs

_showCards = List.intercalate " " . (show <$>) . Set.toAscList

instance Show Play where
  show (Single x) = "<" ++ show x ++ ">"
  show p@(Multiple x) = "<" ++ bomb ++ content ++ ">"
    where bomb = if isBomb p then "!" else ""
          content = _showCards x
  show p@(Straight x) = "<" ++ bomb ++ content ++ ">"
    where bomb = if isBomb p then "!" else ""
          content = _showCards x
  show (FullHouse xs ys) = "<" ++ _showCards xs ++ " " ++ _showCards ys ++ ">"
  show (ConsPairs xs) = "<" ++ (List.intercalate " " . (_showCards <$>) $ xs) ++ ">"

-- | Returns true if the play is a bomb.
isBomb :: Play -> Bool
isBomb (Multiple x) = Set.size x == 4 && not (Set.member Phoenix x)
isBomb (Straight x) = Set.size x >= 5 && uniformSuit x
isBomb _ = False

-- | Return a non-special card that's in the set.  Only useful if you know
-- something about the shape of the set, like it only contains cards of the same
-- rank.
arbitraryStandardCard :: Cards -> Card
arbitraryStandardCard = Set.findMin . Set.filter (not . isSpecialCard)

-- | Builds a Single Play from each Card
constructSingles :: Cards -> Plays
constructSingles = Set.map Single

-- | Return the set of all subsets.
powerset :: Ord a => Set a -> Set (Set a)
powerset x = Set.fromList (Set.fromList <$> List.subsequences (Set.toList x))

-- | Set of all possible Multiple play with a given set of cards
-- allMultiplesOfRank :: Int -> Cards -> Plays
allMultiplesOfRank r =
  Set.map Multiple .
  -- phoenix can't be the 4th card
  Set.filter (\x -> not (Set.size x == 4 && Set.member Phoenix x)) .
  -- 5 card plays are not allowed
  Set.filter ((<5) . Set.size) .
  -- length 0 or 1 sets aren't "multiples"
  Set.filter ((>1) . Set.size) .
  powerset .
  Set.filter (\x -> x == Phoenix || rank x == r)

-- | Return all possible Multiple plays
constructMultiples :: Cards -> Plays
constructMultiples cards = Set.unions (flip allMultiplesOfRank cards <$> allRanks)

-- TODO(jrockway) filter out non-Multiples
-- | Return all possible FullHouse plays, from a set of Multiples
_constructHouses :: Plays -> Plays
_constructHouses ms =
  Set.filter (not . overlaps) . -- get rid of <3 3 * / 2 *>
  Set.filter differentRanks . -- get rid of <3 3 3 / 3 3>
  Set.fromList $ do  -- cartesian products of triples and pairs
    three <- Set.toAscList . Set.filter ((==3) . Set.size) $ cards
    two   <- Set.toAscList . Set.filter ((==2) . Set.size) $ cards
    return $ FullHouse three two
  where
    cards = Set.map unwrapPlay ms
    overlaps (FullHouse xs ys) = (>0) . Set.size $ (xs `Set.intersection` ys)
    differentRanks (FullHouse xs ys) = rank (Multiple xs) /= rank (Multiple ys)

-- | Return all possible FullHouse plays in a set of Cards
constructHouses :: Cards -> Plays
constructHouses = _constructHouses . constructMultiples

-- | Return all possible straights
constructStraights :: Cards -> Plays
constructStraights =
  Set.map Straight .
  Set.filter isStraight .
  Set.filter ((>=5) . Set.size) .
  powerset .
  (`Set.difference` Set.fromList [Dog, Mahjong, Dragon])

-- | Returns true if a set of plays is strictly ascending in rank.
isStrictlyAscending :: Plays -> Bool
isStrictlyAscending xs =
  Set.size xs >= 2 &&
  (getAll . mconcat . map ascends $ zip ranks (tail ranks))
    where ranks = sort . map rank . Set.toAscList $ xs
          ascends (x, y) = All $ y - x == 1

-- | Returns true if the set of plays contains zero or 1 Phoenix
onlyOnePhoenix :: Plays -> Bool
onlyOnePhoenix x = phoenixCount x <= 1
  where phoenixCount = sum . (fromEnum . Set.member Phoenix . unwrapPlay <$>) .
                       Set.toAscList

-- | Return all possible cons pairs, from a set of Multiples
_constructConsPairs :: Plays -> Plays
_constructConsPairs =
  Set.map (ConsPairs . map unwrapPlay . Set.toAscList) .
  Set.filter isStrictlyAscending .
  Set.filter onlyOnePhoenix .
  Set.filter ((>=2) . Set.size) .
  powerset .
  Set.filter ((==2) . Set.size . unwrapPlay)

-- | Return all possible cons paris, from a set of Cards
constructConsPairs :: Cards -> Plays
constructConsPairs = _constructConsPairs . constructMultiples

-- | Return all possible plays
allPlays :: Cards -> Plays
allPlays xs = Set.unions [
  constructSingles xs,
  multiples,
  _constructHouses multiples,
  constructStraights xs,
  _constructConsPairs multiples ]
  where multiples = constructMultiples xs
