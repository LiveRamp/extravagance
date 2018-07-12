module Replacer where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.Applicative


type Meaningless = String
type Meaningful = String

data CharWrap = C String Meaningless deriving (Show)

instance Eq CharWrap where
    (==) (C c1 _ ) (C c2 _ ) = c1 == c2

(<++>) :: Monoid m => RE a m -> RE a m -> RE a m
a <++> b = mappend <$> a <*> b

whiteSpace ::RE Char String
whiteSpace = many $ psym isSpace

interface :: RE Char String
interface = few (psym (not . isSpace)) <++> string "Interface"

punctuation ::RE Char String
punctuation = many $ psym isPunctuation

linecomment :: RE Char String
linecomment = string "//" <++> few anySym <++> string "\n"

multicomment :: RE Char String
multicomment = string "/*" <++> few anySym <++> string "*/"

ignoredForEquality :: RE Char String
ignoredForEquality = join <$> many (linecomment <|> multicomment <|>  whiteSpace <|> punctuation <|> interface)

meaningful :: RE Char String
meaningful = join <$> many (punctuation <|> interface)

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
createReplace (C _ replacement) (C text ignorable) | isJust (findFirstInfix interface ignorable) = C text ignorable
                                                   | otherwise = C text replacement

replaceWrappers :: [CharWrap] -> [CharWrap] -> [CharWrap]
replaceWrappers (replacement : rem) (head : tail)
    | replacement == head = createReplace replacement head : replaceWrappers rem tail
    | otherwise = head : tail
replaceWrappers _ s = s

replaceMatchingStrings :: String -> String -> String
replaceMatchingStrings s1 s2 = unwrapChars (replaceWrappers (wrapChars s1) (wrapChars s2))
