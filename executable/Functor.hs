module Functor where



-- Functor is a typeclass
-- a typeclass, is kinda like an interface
-- when a value is wrapped in a context, you can't apply a normal function to it
-- a functor is any data type that defines how fmap applies to it
-- they let u use a function on a wrapped value
class Functor f where
    fmap :: (a -> b) -> f a -> f b

data Maybe a = Just a | Nothing

data Maybe2 a = Just2 a | Nothing2 deriving Show

--Haskell already defines functors for Maybe, Either, Lists
-- Lets define our own for a tree and fmap for it
--Here is an example tree x = Branch (Tip 7) (Branch (Tip 8) (Tip 9))
data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Prelude.Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (Prelude.fmap func left) (Prelude.fmap func right) 
    