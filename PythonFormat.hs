{-# LANGUAGE OverloadedStrings #-}
module Main (main, formatString) where

import Control.Monad.State hiding (join)
import Data.String.Utils -- MissingH
import Data.List (filter)
import Debug.Trace

type ReaderState = (String, Int, Int, Int) -- out, (, [, {

advanceHead :: String -> State ReaderState String
advanceHead "" = do
    (out, _, _, _) <- get
    return out
    
advanceHead (x:xs) = do
    (out, cp, cb, cc) <- get
    case x of
        '(' -> put (out ++ [x], cp + 1, cb, cc)
        '[' -> put (out ++ [x], cp, cb + 1, cc)
        '{' -> put (out ++ [x], cp, cb, cc + 1)
        ')' -> put (out ++ [x], cp - 1, cb, cc)
        ']' -> put (out ++ [x], cp, cb - 1, cc)
        '}' -> put (out ++ [x], cp, cb, cc - 1)
        ',' -> if cp == 0 && cb == 0 && cc == 0
                 then put (out ++ [x, '\n'], cp, cb, cc)
                 else put (out ++ [x], cp, cb, cc)
        otherwise -> put (out ++ [x], cp, cb, cc)       
    advanceHead xs

startState :: ReaderState
startState = ("", 0, 0, 0)

breakLines :: String -> String
breakLines s = evalState (advanceHead s) startState

addTerminalComma :: [String] -> [String]
addTerminalComma s = let l = last s
                         c = last l :: Char
                         d = case c of
                               ',' -> ""
                               otherwise -> ","
                     in init s ++ [l ++ d]

formatString :: String -> String
formatString = unlines
                . addTerminalComma
                . filter (/= "")
                . map strip
                . lines
                . breakLines

removeTerminalComma :: String -> String
removeTerminalComma s = case (last s :: Char) of
                            ',' -> init s
                            otherwise -> s

joinString :: String -> String 
joinString = removeTerminalComma
               . join " "
               . filter (/= "")
               . map strip
               . lines

joinOrSplit :: String -> String
joinOrSplit s
    | (length . lines) s > 1 = joinString s
    | otherwise = formatString s

main :: IO ()
main = getContents >>= putStr . joinOrSplit
