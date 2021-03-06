module Util where

import Data.Generics

(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&) p1 p2 a = p1 a && p2 a

stripSuffix :: String -> String -> String
stripSuffix suffix (s : tail) | suffix == tail = [s]
                                | otherwise = s : stripSuffix suffix tail
stripSuffix suffix [] = []

modifyIf :: (a -> Bool) -> (a -> a) -> a -> a
modifyIf pred trans a = if pred a then trans a else a

hasAny :: (Data a, Typeable r) => (r -> Bool) -> a -> Bool
hasAny filter obj = not $ null $ listify filter obj
