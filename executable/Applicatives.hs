module Applicatives where

--Applicatives know how to apply a function wrapped in a context to a value wrapped in a context
-- class Functor f => Applicative f where 
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b 

--Lets try to implement it
data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap f (Just2 a) = Just2 (f a)
    fmap f (Nothing2) = Nothing2

instance Applicative Maybe2 where
    pure = Just2
   -- Just2 f <*> (Just2 j) = Just2 (f j)
    Just2 f <*> j = fmap f j -- fmap (+3) (Just2 1)
    Nothing2 <*> j = Nothing2