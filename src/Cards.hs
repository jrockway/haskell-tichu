{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module Cards (
  -- | Represent cards
  Rank(..),
  Suit(..),
  Card(..),
  Cards,
  suit,

  -- | Typeclasses
  Ranked(..),

  -- | Utility functions
  allSuits,
  allRanks,
  isSpecialCard,
  uniformSuit,
  isStraight,

  -- | A standard Tichu deck
  deck,
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Standard Tichu suits.
data Suit = Jade | Pagoda | Sword | Star
          deriving (Ord, Eq, Enum)

instance Show Suit where
  show Jade = "♦"
  show Pagoda = "♣"
  show Sword = "♠"
  show Star = "♥"

allSuits = enumFrom Jade

-- TODO(jrockway): Support fractional ranks (#1).

-- | Represents the rank of a card.  Implements Num so you can use Integer
-- literals where a Rank is required.
newtype Rank = Rank Int
             deriving (Ord, Eq, Num, Enum, Real, Integral)

instance Show Rank where
  show (Rank 14) = "A"
  show (Rank 13) = "K"
  show (Rank 12) = "Q"
  show (Rank 11) = "J"
  show (Rank 10) = "T"
  show (Rank x) = show x

allRanks :: [Rank]
allRanks = [2..14]

-- | Type alias representing a set of cards, like your hand or a deck.
type Cards = Set Card

-- | A Tichu card.
data Card = Dog | Mahjong | Card Rank Suit | Phoenix | Dragon
          deriving (Ord, Eq)

instance Show Card where
  show Dog = "D"
  show Mahjong = "1"
  show (Card r s) = show r ++ show s
  show Phoenix = "*"
  show Dragon = "X"

-- | Return a card's suit, if it has one.
suit :: Card -> Maybe Suit
suit Dog = Nothing
suit Mahjong = Nothing
suit Phoenix = Nothing
suit Dragon = Nothing
suit (Card _ x) = Just x

-- | A set containing all the special cards.
specialCards :: Cards
specialCards = Set.fromList [Dog, Mahjong, Phoenix, Dragon]

-- | Return true if a card is specal.
isSpecialCard :: Card -> Bool
isSpecialCard = (`Set.member` specialCards)

-- | The standard ranked cards, 2 - A in all suits.
standardCards :: Cards
standardCards = Set.fromList $ do
  suit <- allSuits
  rank <- allRanks
  return $ Card rank suit

-- | A standard Tichu deck
deck :: Cards
deck = specialCards `Set.union` standardCards

-- | Returns true if every card in the set is of the same suit.  Special cards
-- make the return value false.
uniformSuit :: Cards -> Bool
uniformSuit x = Set.notMember Nothing suits && Set.size suits == 1
  where suits = Set.map suit x

{- Messy code to determine if a set contains only a straight -}

-- | Returns true if the set represents a straight.
isStraight :: Cards -> Bool
isStraight x = straightResult . foldl compareStraight' (StartState hasPhoenix) .
               Set.toAscList $ cardsWithoutPhoenix
  where hasPhoenix = Set.member Phoenix x
        cardsWithoutPhoenix = Set.delete Phoenix x

-- | Maintains state through the isStraight fold.
data StraightState = StartState { hasPhoenix :: Bool }
                   | StraightState { hasPhoenix :: Bool,
                                     getState :: Bool,
                                     nextCard :: Card }
                   deriving (Show)

straightResult :: StraightState -> Bool
straightResult (StartState _) = False
straightResult (StraightState {getState}) = getState

-- | The fold step of a routine that determines whether a set of cards is a
-- straight.  The Phoenix can fill a gap exactly once.
compareStraight' :: StraightState -> Card -> StraightState
compareStraight' (StartState {hasPhoenix}) card = (StraightState hasPhoenix True card)
compareStraight' (StraightState {hasPhoenix, getState=False, nextCard=_}) next =
  StraightState hasPhoenix False next
compareStraight' (StraightState False True low) high = -- non-phoenix case
  StraightState False (rank high - rank low == 1) high
compareStraight' (StraightState True True low) high = -- with phoenix case
  StraightState hasPhoenix' status high
    where difference = rank high - rank low
          status = difference == 1 || difference == 2
          usedPhoenix = difference == 2
          hasPhoenix' = not usedPhoenix


-- | Type whose values can have a rank.
class Ranked a where
  -- | Return the rank of the object.
  rank :: a -> Rank

instance Ranked Rank where
  rank x = x

-- | Note that we make up ranks for the special cards; this only influences
-- display order, actual win logic is handled in terms of the Play data
-- structure.
instance Ranked Card where
  rank Dog = 0
  rank Mahjong = 1
  rank (Card x _) = x
  rank Phoenix = 100
  rank Dragon = 1000
