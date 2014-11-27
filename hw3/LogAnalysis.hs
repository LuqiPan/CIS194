{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List
import Data.Char

parseMessage :: String -> MaybeLogMessage
parseMessage [] = InvalidLM []
parseMessage (msgType : rstMsg) =
  case (msgType, maybeNumber) of
    ('I', ValidInt n) -> ValidLM (LogMessage Info n (unwords rst))
    ('W', ValidInt n) -> ValidLM (LogMessage Warning n (unwords rst))
    ('E', ValidInt n) ->
      let anotherNumberStr : errorRst = rst
          maybeAnotherNumber = readInt anotherNumberStr
      in case maybeAnotherNumber of
        ValidInt m -> ValidLM (LogMessage (Error n) m (unwords errorRst))
        InvalidInt -> InvalidLM (msgType : rstMsg)
    _ -> InvalidLM (msgType : rstMsg)
  where
    numberStr : rst = words rstMsg
    maybeNumber = readInt numberStr

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (x:ys) =
  case x of
    ValidLM l -> l:validMessagesOnly ys
    InvalidLM _ -> validMessagesOnly ys

parse :: String -> [LogMessage]
parse logFile =
  let logLines = lines logFile
  in
  validMessagesOnly (map parseMessage logLines)

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _)
  | ts1 < ts2 = LT
  | ts1 == ts2 = EQ
  | otherwise = GT

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _)
  | level >= 50 = True
  | otherwise = False
isSevere _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  map (\(LogMessage _ _ content) -> content) (filter isSevere (sortMessages msgs))

isInfo :: LogMessage -> Bool
isInfo (LogMessage Info _ _) = True
isInfo _ = False

isntInfo :: LogMessage -> Bool
isntInfo msg = not (isInfo msg)

isAbout :: String -> LogMessage -> Bool
isAbout keyword (LogMessage _ _ content) =
  map toLower keyword `isInfixOf` map toLower content

andCombinator :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
andCombinator pred1 pred2 msg =
  pred1 msg && pred2 msg

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout keyword = filter (andCombinator (isAbout keyword) isntInfo)

orCombinator :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
orCombinator pred1 pred2 msg =
  pred1 msg || pred2 msg

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced keyword msgs =
  map (\(LogMessage _ _ content) -> content)
    (filter (orCombinator isSevere (isAbout keyword)) msgs)
