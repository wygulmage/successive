{-# LANGUAGE DefaultSignatures
  #-}


module Data.Successive (
    Successive (..),
    clampDec, clampInc,
    -- isMax, isMin,
    inc, dec,
    decFrom, incFrom,
    decFromTo, incFromTo,
    enumerateDown, enumerateUp,
    ) where


import Data.Fixed (Fixed)
import Data.Int
import Data.Word
import qualified Data.List.NonEmpty as NonEmpty

import Foreign.C.Types
   (CBool, CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong)

import Numeric.Natural
import qualified GHC.Natural as Natural (naturalToWordMaybe)


-- TODO: Should Successive required Ord? Should in NOT require Eq?


{- |
@'dec' x@ gives 'Just' the value closest to but smaller than @x@, or 'Nothing' if @x@ is the minimum value.
@'inc' x@ gives 'Just' the value closest to but larger than @x@, or 'Nothing' if @x@ is the maximum value.
A @Successive@ type may be bounded or unbounded, and you can determine whether a value is the maximum or minimum bound.

Sensible defaults definitions are provided for types that are 'Bounded' and 'Enum'. For types that are not 'Bounded', you must provide definitions of 'isMax' and 'isMin'. For types that are not 'Enum', you must provide definitions of 'uncheckedDec' and 'uncheckedInc'.

Although it is easy to define instances in terms of 'uncheckedDec' and 'uncheckedInc', those functions should rarely be used. 'dec', 'inc', 'clampedDec', and 'clampedInc' are provided as more convenient ways to use @Successive@ types.

Laws:
(Ord a)=>
   clampDec x `min` x = clampDec x
   clampInc x `min` x = x
   clampDec x `max` x = x
   clampInc x `max` x = clampInc x
-}
class (Eq a)=> Successive a where
  isMax :: a -> Bool
  isMin :: a -> Bool
  uncheckedDec :: a -> a
  uncheckedInc :: a -> a

  default isMax :: (Bounded a)=> a -> Bool
  default isMin :: (Bounded a)=> a -> Bool
  isMax = (maxBound ==)
  isMin = (minBound ==)

  default uncheckedDec :: (Enum a)=> a -> a
  default uncheckedInc :: (Enum a)=> a -> a
  uncheckedDec = pred
  uncheckedInc = succ


dec, inc :: (Successive a)=> a -> Maybe a
dec x
   | isMin x = Nothing
   | otherwise = Just $ uncheckedDec x
inc x
   | isMax x = Nothing
   | otherwise = Just $ uncheckedInc x

clampDec, clampInc :: (Successive a)=> a -> a
clampDec x
   | isMin x = x
   | otherwise = uncheckedDec x
clampInc x
   | isMax x = x
   | otherwise = uncheckedInc x

decFrom, incFrom :: (Successive a)=> a -> NonEmpty.NonEmpty a
decFrom = iterateMaybe dec
{-# INLINABLE decFrom #-}
incFrom = iterateMaybe inc
{-# INLINABLE incFrom #-}

decFromTo, incFromTo :: (Successive a)=> a -> a -> [a]
decFromTo x0 xN = takeUntil (xN ==) $ decFrom x0
incFromTo x0 xN = takeUntil (xN ==) $ incFrom x0
{-# INLINABLE incFromTo #-}
{-# INLINABLE decFromTo #-}

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
instance Successive CBool
instance Successive CChar
instance Successive CSChar
instance Successive CUChar
instance Successive CShort
instance Successive CUShort
instance Successive CInt
instance Successive CUInt
instance Successive CLong
instance Successive CULong
instance Successive CLLong
instance Successive CULLong

instance Successive Natural where
  isMax _ = False
  isMin x = case Natural.naturalToWordMaybe x of
     Just w  -> 0 == w
     Nothing -> False

instance Successive Integer where
  isMax _ = False
  isMin _ = False

instance Successive (Fixed a) where
  isMax _ = False
  isMin _ = False


--- Helpers (not exported)

iterateMaybe :: (a -> Maybe a) -> a -> NonEmpty.NonEmpty a
{- ^
@iterateMaybe f x@ is the 'NonEmpty' list produced by iterating @f@ from @x@ until @f x'@ is @Nothing@.
-}
iterateMaybe f = NonEmpty.unfoldr (\ x -> (x, f x))
{-# INLINE iterateMaybe #-}

takeUntil :: (Foldable m)=> (a -> Bool) -> m a -> [a]
{- ^
@takeUntil p xs@ collects values of @xs@ until @p@ is satisfied. For example, @last . takeUntil p@ is equivalent to @'find' p@.
-}
takeUntil p = foldr (\ x xs -> x : if p x then [] else xs) []
{-# INLINE takeUntil #-}


{- NOTE: Could interdefine methods in a couple ways:

MINIMAL: (inc | (isMax, uncheckedInc)), (dec | (uncheckedDec, isMin))
isMax = isMaxDefault
isMin = isMinDefault
dec = decDefault
inc = incDefault
uncheckedDec = uncheckedDecDefault
uncheckedInc = uncheckedIncDefault

MINIMAL: (isMax, uncheckedInc, uncheckedDec, isMin)
isMax = isMaxBounded
isMin = isMinBounded
uncheckedDec = pred
uncheckedInc = succ

MINIMAL: (inc, dec)
inc = incBoundedEnum
dec = decBoundedEnum
isMax = isMaxDefault
isMin = isMinDefault
uncheckedDec = uncheckedDecDefault
uncheckedInc = uncheckedIncDefault


decDefault x
   | isMin x   = Nothing
   | otherwise = Just $ uncheckedDec x
uncheckedDecDefault = unJust . dec
isMinDefault = null . dec

incDefault x
   | isMax x   = Nothing
   | otherwise = Just $ uncheckedInc x
uncheckedIncDefault = unJust . inc
isMaxDefault = null . inc

isMaxBounded = (maxBound ==)
isMinBounded = (minBound ==)

uncheckedIncEnum = succ
uncheckedDecEnum = pred

incBoundedEnum x
   | isMaxBounded x = Nothing
   | otherwise      = Just $ uncheckedIncEnum x
decBoundedEnum x
   | isMinBounded x = Nothing
   | otherwise      = Just $ uncheckedDecEnum x

-}
