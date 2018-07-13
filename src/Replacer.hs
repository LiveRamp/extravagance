{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Replacer where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.Applicative
import           Util

data Section = Section {
    hash :: String,
    text :: String
} deriving (Show)

instance Eq Section where
    (==) Section {hash = hash1} Section {hash = hash2} = hash1 == hash2

(<++>) :: Monoid m => RE a m -> RE a m -> RE a m
a <++> b = mappend <$> a <*> b

inSet :: String -> Char -> Bool
inSet = flip elem

symIn :: String -> RE Char Char
symIn s = psym (`elem` s)

symNotIn :: String -> RE Char Char
symNotIn s = psym (not . inSet s)

whiteSpace :: RE Char String
whiteSpace = some $ psym isSpace

isPunc :: Char -> Bool
isPunc = isPunctuation .&& (/= '_')

punctuation ::RE Char String
punctuation = some $ psym isPunc

notWSOrPunc :: RE Char Char
notWSOrPunc = psym ((not . isSpace) .&& (not . isPunc))

linecomment :: RE Char String
linecomment =  "//" <++> few anySym <++> "\n"

multicomment :: RE Char String
multicomment =  "/*" <++> few anySym <++> "*/"

ignoredForEquality :: RE Char String
ignoredForEquality = join <$> many (linecomment <|> multicomment <|> whiteSpace <|> punctuation)

sectionMatcher :: RE Char Section
sectionMatcher = do
    ignored <- ignoredForEquality
    value <- many notWSOrPunc
    return Section {hash = value, text = ignored ++ value}

sectionize :: String -> Maybe [Section]
sectionize = match (some sectionMatcher)

concatSections :: [Section] -> String
concatSections []                     = []
concatSections (Section{text} : tail) =  text ++ concatSections tail

mergeSections' :: Integer -> Bool -> [Section] -> [Section] -> [Section]
mergeSections' depth forceKeep (replacement : rem) (head : tail)
    | replacement == head = (if forceKeep then head else replacement) : mergeSections' 0 False rem tail
    | depth < 100 = head : mergeSections' (depth + 1) True (replacement : rem) tail
    | otherwise = head : tail
mergeSections' _ _ _ orig = orig

mergeSections = mergeSections' 0 False

replaceMatchingStrings' :: String -> String -> Maybe String
replaceMatchingStrings' replacer original = do
    sec1 <-  sectionize replacer
    sec2 <-  sectionize original
    return $ concatSections (mergeSections sec1 sec2)

replaceMatchingStrings :: String -> String -> String
replaceMatchingStrings replacer original = fromMaybe original replaced where
    replaced = replaceMatchingStrings' replacer original
