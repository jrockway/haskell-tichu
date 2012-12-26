module Rules (
  isLegalPlay,
  ) where

import Cards
import Plays

-- | Given two plays, return true if the second play can be played over the first.
isLegalPlay :: Play -> Play -> Bool

-- Non-bombs can always be bombed
isLegalPlay x y | (not . isBomb) x && isBomb y = True

-- Longer bombs beat bombs
isLegalPlay x y | isBomb x && isBomb y && playLength x < playLength y = True

-- Mahjong and Dog can only be lead
isLegalPlay _ (Single Mahjong) = False
isLegalPlay _ (Single Dog) = False

-- Excepting bombs, nothing beats the Dragon
isLegalPlay (Single Dragon) _ = False

-- The Phoenix beats everything
isLegalPlay _ (Single Phoenix) = True

-- But the Phoenix is beaten by everything
isLegalPlay (Single Phoenix) _ = True

-- Singles increase in rank
isLegalPlay x@(Single _) y@(Single _) = rank x < rank y

-- Multiples increase in rank but stay the same size
isLegalPlay x@(Multiple _) y@(Multiple _) =
  rank x < rank y && playLength x == playLength y

-- Full houses increase in rank
isLegalPlay x@(FullHouse _ _) y@(FullHouse _ _) = rank x < rank y

-- Straights increase in rank, but can be bombed by longer bombs if they are
-- bombs.
isLegalPlay x@(Straight _) y@(Straight _) | isBomb x && isBomb y =
   rank x < rank y || playLength x < playLength y
isLegalPlay x@(Straight _) y@(Straight _) = rank x < rank y

-- Cons pairs increase in rank but stay the same size
isLegalPlay x@(ConsPairs _) y@(ConsPairs _) =
  rank x < rank y && playLength x == playLength y

isLegalPlay _ _ = False
