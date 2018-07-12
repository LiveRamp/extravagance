{-# LANGUAGE OverloadedStrings #-}

module Replacer where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.Applicative


type Meaningless = String

data CharWrap = C String Meaningless deriving (Show)

instance Eq CharWrap where
    (==) (C c1 _ ) (C c2 _ ) = c1 == c2

(<++>) :: Monoid m => RE a m -> RE a m -> RE a m
a <++> b = mappend <$> a <*> b

symIn :: String -> RE Char Char
symIn s = psym (`elem` s)

symNotIn :: String -> RE Char Char
symNotIn s = psym (not . (`elem` s))

whiteSpace ::RE Char String
whiteSpace = some $ psym isSpace

interface :: RE Char String
interface = few (psym (not . isSpace)) <++> "Interface"

punctuation ::RE Char String
punctuation = some $ symIn ",()"

linecomment :: RE Char String
linecomment =  "//" <++> few anySym <++> "\n"

multicomment :: RE Char String
multicomment =  "/*" <++> few anySym <++> "*/"

preHook :: RE Char String
preHook = "pre_" <++> many (symNotIn "\n") <++> "\n"

ignoredForEquality :: RE Char String
ignoredForEquality = join <$> many (linecomment <|> multicomment <|> whiteSpace <|> 
                                    punctuation <|> interface <|> preHook)

meaningful :: RE Char String
meaningful = join <$> some (some (sym ',') <|> interface <|> preHook)

wrapChars :: String -> [CharWrap]
wrapChars [] = []
wrapChars input      = result : wrapChars rest  where
    parsed = findFirstPrefix ignoredForEquality input
    (result, rest) = makeResult input parsed

makeResult :: String -> Maybe (String, String) -> (CharWrap, String)
makeResult input (Just (meaningless, h : rest)) = (C [h] meaningless, rest)
makeResult input (Just (meaningless, []))       = (C "" meaningless,"")
makeResult (h : rest) Nothing                   = (C [h] "", rest)

unwrapChars :: [CharWrap] -> String
unwrapChars []                       = []
unwrapChars (C c meaningless : tail) =  meaningless ++ c ++ unwrapChars tail

createReplace :: CharWrap -> CharWrap -> CharWrap
createReplace (C _ replacement) (C text ignorable) | isJust (findFirstInfix meaningful ignorable) = C text (traceShowId ignorable)
                                                   | otherwise = C text replacement

replaceWrappers :: [CharWrap] -> [CharWrap] -> [CharWrap]
replaceWrappers (replacement : rem) (head : tail)
    |  traceShowId replacement ==  traceShowId head = createReplace replacement head : replaceWrappers rem tail
    | otherwise = head : tail
replaceWrappers _ s = s

replaceMatchingStrings :: String -> String -> String
replaceMatchingStrings s1 s2 = unwrapChars (replaceWrappers (wrapChars s1) (wrapChars s2))
