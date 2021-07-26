{-# LANGUAGE DefaultSignatures
  #-}


module Data.Successive (
    Successive (..),
    clampDec, clampInc,
    isMax, isMin,
    decFrom, incFrom,
    decWhile, incWhile,
    ) where


import Prelude hiding (pred, succ)
import qualified Prelude as Enum

import Data.Fixed (Fixed)
import Data.Int
import Data.Word
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)

import Numeric.Natural

-- TODO: Should Successive required Ord? Should in NOT require Eq?
-- TODO: Should Successive have any other methods (e.g. isMax, isMin, uncheckedInc, uncheckedDec)?

{- |
A @Successive@ type may be bounded or unbounded, and you can determine whether a value is the maximum or minimum bound.
-}
class (Eq a)=> Successive a where
  inc :: a -> Maybe a
  dec :: a -> Maybe a

  -- isMin :: a -> Bool
  -- isMax :: a -> Bool

  -- default isMax :: (Bounded a, Eq a)=> a -> Bool
  -- default isMin :: (Bounded a, Eq a)=> a -> Bool
  -- isMin = (minBound ==)
  -- isMax = (maxBound ==)

  -- default pred :: (Enum a)=> a -> Maybe a
  -- default succ :: (Enum a)=> a -> Maybe a
  -- pred x
  --    | isMin x   = Nothing
  --    | otherwise = Just $ Enum.pred x

  -- succ x
  --    | isMax x   = Nothing
  --    | otherwise = Just $ Enum.succ x
  default inc :: (Bounded a, Enum a)=> a -> Maybe a
  default dec :: (Bounded a, Enum a)=> a -> Maybe a
  inc = incBounded
  dec = incBounded


clampDec, clampInc :: (Successive a)=> a -> a
clampDec x = fromMaybe x (dec x)
clampInc x = fromMaybe x (inc x)

isMin, isMax :: (Successive a)=> a -> Bool
isMin = null . dec
isMax = null . inc

decFrom, incFrom :: (Successive a)=> a -> NonEmpty.NonEmpty a
decFrom = iterateMaybe dec
{-# INLINABLE decFrom #-}
incFrom = iterateMaybe inc
{-# INLINABLE incFrom #-}

incFromTo x0 xN = takeUntil (xN ==) $ incFrom x0
decFromTo x0 xN = takeUntil (xN ==) $ decFrom x0

decWhile, incWhile :: (Successive a)=> (a -> Bool) -> a -> [a]
decWhile p = NonEmpty.takeWhile p . decFrom
incWhile p = NonEmpty.takeWhile p . incFrom

enumerateDown, enumerateUp :: (Bounded a, Successive a)=> NonEmpty.NonEmpty a
enumerateDown = decFrom maxBound
enumerateUp = incFrom minBound


instance Successive ()
instance Successive Bool
instance Successive Ordering
instance Successive Char
instance Successive Int
instance Successive Int8
instance Successive Int16
instance Successive Int32
instance Successive Int64
instance Successive Word
instance Successive Word8
instance Successive Word16
instance Successive Word32
instance Successive Word64

instance Successive Natural where
  dec x
     | x == 0 = Nothing
     | otherwise = Just $ x - 1

  inc = incUnbounded

instance Successive Integer where
  dec = decUnbounded
  inc = incUnbounded

instance Successive (Fixed a) where
  dec = decUnbounded
  inc = incUnbounded


isMaxBounded, isMinBounded :: (Bounded a, Eq a, Enum a)=> a -> Bool
isMaxBounded = (maxBound ==)
isMinBounded = (minBound ==)

decBounded, incBounded :: (Bounded a, Eq a, Enum a)=> a -> Maybe a
decBounded x
     | isMinBounded x = Nothing
     | otherwise      = Just $ Enum.succ x

incBounded x
     | isMaxBounded x = Nothing
     | otherwise      = Just $ Enum.pred x

decUnbounded, incUnbounded :: (Enum a)=> a -> Maybe a
decUnbounded = Just . Enum.pred
incUnbounded = Just . Enum.succ

iterateMaybe :: (a -> Maybe a) -> a -> NonEmpty.NonEmpty a
iterateMaybe f = NonEmpty.unfoldr (\ x -> (x, f x))

takeUntil :: (Foldable m)=> (a -> Bool) -> m a -> [a]
{- ^
@takeUntil p xs@ collects values of @xs@ until @p@ is satisfied. For example, @last . takeUntil p@ is equivalent to @'find' p@.
-}
takeUntil p = foldr (\ x xs -> x : if p x then [] else xs) []
{-# INLINE takeUntil #-}
