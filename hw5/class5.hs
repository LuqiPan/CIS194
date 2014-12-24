{-# LANGUAGE FlexibleInstances #-}

module Class5 where

import Data.Char  ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read  ( readMaybe )

listToUpper :: [String] -> [String]
listToUpper = map $ map toUpper
