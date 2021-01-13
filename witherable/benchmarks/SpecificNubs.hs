{-# LANGUAGE DeriveFunctor #-}
module SpecificNubs where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HSet

listHashNub :: (Eq a, Hashable a) => [a] -> [a]
listHashNub = listHashNubOn id
{-# INLINE listHashNub #-}

listHashNubOn :: (Eq b, Hashable b) => (a -> b) -> [a] -> [a]
listHashNubOn p = go HSet.empty
  where
    go _ []     = []
    go s (x:xs) = case HM.alterF g (p x) (HSet.toMap s) of
        BoolPair True  s' ->     go (HSet.fromMap s') xs
        BoolPair False s' -> x : go (HSet.fromMap s') xs
      where
        g Nothing  = BoolPair False (Just ())
        g (Just _) = BoolPair True  (Just ())
{-# INLINE listHashNubOn #-}

-- used to implement *Nub functions.
data BoolPair a = BoolPair !Bool a deriving Functor
