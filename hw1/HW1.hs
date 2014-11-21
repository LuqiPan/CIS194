{-# OPTIONS_GHC -Wall #-}

{-
Name: Luqi Pan
Collaborators: None
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

lastDigit :: Integer -> Integer
lastDigit n =
  mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n =
  div n 10

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (toDigits (dropLastDigit n)) ++ [(lastDigit n)]

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:[]) = x:[]
doubleEveryOtherHelper (x:y:zs) = x:(2*y):(doubleEveryOtherHelper zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  reverse (doubleEveryOtherHelper (reverse xs))

numberSum :: Integer -> Integer
numberSum n
  | (0 <= n) && (n < 10) = n
  | (9 < n) && (n < 19) = (mod n 10) + 1
  | otherwise = -65536

sumDigits :: [Integer] -> Integer
sumDigits xs =
  sum (map numberSum xs)

validate :: Integer -> Bool
validate cardNumber =
  mod (sumDigits (doubleEveryOther (toDigits cardNumber))) 10 == 0
