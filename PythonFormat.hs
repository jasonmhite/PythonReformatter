{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Monad.State hiding (join)
import Data.String.Utils -- MissingH
import Data.List (filter)
import Data.Char (isSpace)

-- TODO : Need to read the initial indent and preserve that
-- TODO : Also need to preserve trailing characters after the close is found

type ReaderState = (String, String, String, Int, Int, Int) -- out, close, indent, (, [, {

advanceHead :: String -> State ReaderState (String, String)
advanceHead "" = do
    (out, _, ind, _, _, _) <- get
    return (out, ind)
    
-- Effectively a state machine that reads characters off a tape, hence the name
advanceHead (x:xs) = do
    (out, cl, ind, cp, cb, cc) <- get
    if | isSpace x && cl == "" && out == "" -> put (out, cl, ind ++ [x], cp, cb, cc)
       | cl == "" -> case x of
            '(' -> put (out ++ [x] ++ "\n", ")", ind, cp, cb, cc)
            '[' -> put (out ++ [x] ++ "\n", "]", ind, cp, cb, cc)
            '{' -> put (out ++ [x] ++ "\n", "}", ind, cp, cb, cc)
            otherwise -> put (out ++ [x], cl, ind, cp, cb, cc)
       | x == ')' && cl == ")" && cp == 0 ->
            put (out ++ "\n" ++ [x], cl, ind, cp - 1, cb, cc)
       | x == ']' && cl == "]" && cb == 0 ->
            put (out ++ "\n" ++ [x], cl, ind, cp, cb - 1, cc) 
       | x == '}' && cl == "}" && cc == 0 ->
            put (out ++ "\n" ++ [x], cl, ind, cp, cb, cc - 1)  
       | otherwise -> case x of
            '(' -> put (out ++ [x], cl, ind, cp + 1, cb, cc)
            '[' -> put (out ++ [x], cl, ind, cp, cb + 1, cc)
            '{' -> put (out ++ [x], cl, ind, cp, cb, cc + 1)
            ')' -> put (out ++ [x], cl, ind, cp - 1, cb, cc)
            ']' -> put (out ++ [x], cl, ind, cp, cb - 1, cc)
            '}' -> put (out ++ [x], cl, ind, cp, cb, cc - 1)
            ',' -> if cp == 0 && cb == 0 && cc == 0
                     then put (out ++ [x, '\n'], cl, ind, cp, cb, cc)
                     else put (out ++ [x], cl, ind, cp, cb, cc)
            otherwise -> put (out ++ [x], cl, ind, cp, cb, cc)       
    advanceHead xs 

startState :: ReaderState
startState = ("", "", "", 0, 0, 0)

breakLines :: String -> (String, String)
breakLines s = evalState (advanceHead s) startState

addTerminalComma :: [String] -> [String]
addTerminalComma [] = []
addTerminalComma [x] = [x]
addTerminalComma [x, y] = [x, y]
addTerminalComma x = let fline = head x
                         lline = last x
                         body = (init . tail) x
                         insBody = init body
                         endBody = (rstrip . last) body
                         newBody = if (not . endswith ",") endBody
                                     then endBody ++ ","
                                     else endBody
                     in [fline]
                          ++ insBody
                          ++ [newBody]
                          ++ [lline]

indentChar :: String
indentChar = "    "

-- The second and third patterns are probably not needed
indentLines :: [String] -> [String]
indentLines [] = []
indentLines [x] = [x]
indentLines [x, y] = [x, y]
indentLines x  = let fline = head x
                     lline = last x
                     ins = (init . tail) x 
                 in [fline]
                      ++ map (indentChar ++) ins
                      ++ [lline]

z (x, y) = (lines x, y)

-- TODO : Reformat to use a state monad, make the composes binds
formatString :: String -> String
formatString = unlines
                . addTerminalComma
                . indentLines
                . (\(x, i) -> map (i ++) x)
                . (\(x, i) -> (filter (/= "") x, i))
                . (\(x, i) -> (map strip x, i))
                . (\(x, i)  -> (lines x, i))
                . breakLines

-- Currently a bit broken, at least as a vim filter, hence disabled
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
    {-| (length . lines) l > 1 = joinString l-}
    | otherwise = formatString l
    where l = rstrip s

main :: IO ()
main = getContents >>= putStr . joinOrSplit
