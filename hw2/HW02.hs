{-# OPTIONS_GHC -Wall #-}

{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:ys) hand =
  if (elem x hand) then formableBy ys (delete x hand)
  else False

wordsFromHelper :: Hand -> [String] -> [String] -> [String]
wordsFromHelper _ acc [] = acc
wordsFromHelper hand acc (x:ys) =
  if (formableBy x hand) then (wordsFromHelper hand (x:acc) ys)
  else (wordsFromHelper hand acc ys)

wordsFrom :: Hand -> [String]
{-wordsFrom hand = filter (`formableBy` hand) allWords-}
wordsFrom hand = wordsFromHelper hand [] allWords

wordMatchTemplate :: Template -> String -> Bool
wordMatchTemplate [] [] = True
wordMatchTemplate [] _ = False
wordMatchTemplate _ [] = False
wordMatchTemplate (a:bs) (x:ys) =
  if a == x then (wordMatchTemplate bs ys)
  else
    if a == '?' then (wordMatchTemplate bs ys)
    else False

wordNeedMatchHelper :: Template -> String -> String -> String
wordNeedMatchHelper [] [] acc = acc
wordNeedMatchHelper [] _ _ = ""
wordNeedMatchHelper _ [] _ = ""
wordNeedMatchHelper (a:bs) (x:ys) acc =
  if (a == '?') then (wordNeedMatchHelper bs ys (x:acc))
  else (wordNeedMatchHelper bs ys acc)

wordNeedMatch :: Template -> String -> String
wordNeedMatch temp word = wordNeedMatchHelper temp word ""

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate temp hand word =
  if (wordMatchTemplate temp word) then
    (formableBy (wordNeedMatch temp word) hand)
  else False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate temp hand =
  filter (wordFitsTemplate temp hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord word = sum (map scrabbleValue word)

bestWords :: [String] -> [String]
bestWords [] = []
bestWords wordLst =
  filter (\x -> (scrabbleValueWord x) == maxValue) wordLst
    where maxValue = maximum (map scrabbleValueWord wordLst)
