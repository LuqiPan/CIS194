{-# OPTIONS_GHC -Wall #-}

module HW05 where

import Ring
import Parser

intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                  (addId == (0 :: Integer))

data Mod5 = MkMod Integer
  deriving (Show, Eq)

instance Ring Mod5 where
  addId = MkMod(0)
  addInv (MkMod(n)) = MkMod(5 - n)
  mulId = MkMod(1)

  add (MkMod(a)) (MkMod(b)) = MkMod((a+b) `mod` 5)
  mul (MkMod(a)) (MkMod(b)) = MkMod((a*b) `mod` 5)

instance Parsable Integer where
