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

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 from to _ = [(from, to)]
hanoi n from to temp =
  (hanoi (n-1) from temp to) ++ [(from, to)] ++ (hanoi (n-1) temp to from)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 from to _ _ = [(from, to)]
hanoi4 2 from to temp1 _ = hanoi 2 from to temp1
hanoi4 n from to temp1 temp2 =
  hanoi4 (div n 2) from temp1 to temp2 ++
  hanoi (n - (div n 2)) from to temp2 ++
  hanoi4 (div n 2) temp1 to from temp2
