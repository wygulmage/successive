{-# LANGUAGE DefaultSignatures
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
  #-}


module Data.Successive (
    Successive (..),
    dec, inc,
    clampDec, clampInc,
    decFrom, incFrom,
    decFromTo, incFromTo,
    enumerateFromTo,
    enumerateDown, enumerateUp,
    ) where


import Data.Fixed (Fixed)
import Data.Int
import Data.Proxy (Proxy)
import Data.Word
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Const (Const (Const))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Classes (Ord1)
import Data.Ord (Down (Down))
import Data.Monoid (Alt (Alt), Ap (Ap))
import Data.Semigroup
   (Any (Any), All (All), First (First), Last (Last), Max (Max), Min (Min))

import Foreign.C.Types
   (CBool, CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong, CPtrdiff, CIntPtr, CUIntPtr, CSize, CWchar, CSigAtomic)

import Numeric.Natural
import qualified GHC.Natural as GHC (naturalToWordMaybe)


-- TODO: Should `Successive` provide a way to determine whether there is a upper or lower bound (e.g. `hasMax, hasMin :: proxy a -> Bool`)?


{- |
A @Successive@ type may be bounded or unbounded, and you can determine whether a value is the maximum or minimum bound.

Sensible defaults definitions are provided for types that are 'Bounded' and 'Enum'. For types that are not 'Bounded', you must provide definitions of 'isMax' and 'isMin'. For types that are not 'Enum', you must provide definitions of 'uncheckedDec' and 'uncheckedInc'.

Although it is easy to define instances in terms of 'uncheckedDec' and 'uncheckedInc', those functions should rarely be used. 'dec', 'inc', 'clampDec', and 'clampInc' are provided as more convenient ways to use @Successive@ types.

@'uncheckedDec' x@ when @'isMin' x@ throws an error or silently return an incorrect value. Which it does is an internal instance-specific implemntation detail. If you want to guarantee an error, use @'Data.Maybe.unJust' . 'dec'@.

@'uncheckedInc' x@ when @'isMax' x@ throws an error or silently return an incorrect value. Which it does is an internal instance-specific implemntation detail. If you want to guarantee an error, use @'Data.Maybe.unJust' . 'inc'@.

Laws:

   @'clampDec' x `'min'` x@ = @clampDec x@

   @'clampInc' x `'min'` x@ = @x@

   @clampDec x `'max'` x@ = @x@

   @clampInc x `'max'` x@ = @clampInc x@

Justification:
@Successive@ exists to take the guesswork out of 'Enum'\'s 'pred' and 'succ' while still being convenient to define and use.

This does not include the ability to write enumerations with step sizes. That really is out of scope, and belongs in a numeric subclass. I think @'enumFromTo' 'Data.Char.UppercaseLetter' 'Data.Char.ParagraphSeparator'@ is clear in context and useful, but the meaning of @'enumFromThen' 'Data.Char.TitlecaseLetter' 'Data.Char.EnclosingMark'@ is just too unclear.
-}
class (Ord a)=> Successive a where
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


{- |
@dec@ returns 'Just' the next lower value, or 'Nothing' if applied to the minimum value.
@inc@ returns 'Just' the next higher value, or 'Nothing' if applied to the maximum value.
-}
dec, inc :: (Successive a)=> a -> Maybe a
dec x
   | isMin x = Nothing
   | otherwise = Just $ uncheckedDec x
inc x
   | isMax x = Nothing
   | otherwise = Just $ uncheckedInc x
{-# INLINABLE dec #-}
{-# INLINABLE inc #-}

{- |
@clampDec x@ returns @x@ if x is the minimum value, otherwise it decrements x.
@clampInc x@ returns @x@ if x is the maximum value, otherwise it increments x.
-}
clampDec, clampInc :: (Successive a)=> a -> a
clampDec x
   | isMin x = x
   | otherwise = uncheckedDec x
clampInc x
   | isMax x = x
   | otherwise = uncheckedInc x
{-# INLINABLE clampDec #-}
{-# INLINABLE clampInc #-}

-- clampDecTo, clampIncTo :: (Successive a)=> a -> a -> a
-- clampDecTo limit x
--    | limit < x = uncheckedDec x
--    | otherwise = limit
-- clampIncTo limit x
--   | limit > x = uncheckedInc x
--   | otherwise = limit

decFrom, incFrom :: (Successive a)=> a -> NonEmpty.NonEmpty a
{- ^
@decFrom x@ is the 'NonEmpty.NonEmpty' list of all values less than or equal to @x@, in descending order.
@incFrom x@ is the 'NonEmpty.NonEmpty' list of all values greater than or equal to @x@, in ascending order.
-}
decFrom = iterateMaybe dec
{-# INLINABLE decFrom #-}
incFrom = iterateMaybe inc
{-# INLINABLE incFrom #-}

decFromTo, incFromTo :: (Successive a)=> a -> a -> [a]
{- ^
@decFromTo start end@ and @incFromTo start end@ give all the values from @start@ to @end@, including @start@ and @end@.
-}
decFromTo x0 xN = NonEmpty.takeWhile (xN >=) $ decFrom x0
incFromTo x0 xN = NonEmpty.takeWhile (xN <=) $ incFrom x0
{-# INLINABLE incFromTo #-}
{-# INLINABLE decFromTo #-}

enumerateFromTo :: (Successive a)=> a -> a -> NonEmpty.NonEmpty a
{- ^
@enumateFromTo start end@ is the 'NonEmpty.NonEmpty' list of all values from start to end. If @start@ is less than @end@, the values are in ascending order; if it is greater, the values are in descending order. (If they are equal, you just get the singleton list of @start@.)
-}
enumerateFromTo start end
   | start <= end
   = NonEmpty.unfoldr
      (\ x -> (x, if x < end then Just (uncheckedInc x) else Nothing))
      start
   | otherwise
   = NonEmpty.unfoldr
      (\ x -> (x, if x > end then Just (uncheckedDec x) else Nothing))
      start

enumerateDown, enumerateUp :: (Bounded a, Successive a)=> NonEmpty.NonEmpty a
{- ^
@enumerateDown@ and @enumerateUp@ are 'NonEmpty.NonEmpty' lists of all values of their type, ordered from largest to small and smallest to largest, respectively.
-}
enumerateDown = decFrom maxBound
enumerateUp = incFrom minBound


instance Successive ()
instance Successive Ordering
instance Successive Char
instance Successive Int8
instance Successive Int16
instance Successive Int32
instance Successive Int64
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
instance Successive CPtrdiff
instance Successive CIntPtr
instance Successive CUIntPtr
instance Successive CSize
instance Successive CWchar
instance Successive CSigAtomic
instance Successive (Proxy a)

deriving instance Successive Any
deriving instance Successive All
deriving instance (Successive a)=> Successive (Identity a)
deriving instance (Successive a)=> Successive (First a)
deriving instance (Successive a)=> Successive (Last a)
deriving instance (Successive a)=> Successive (Max a)
deriving instance (Successive a)=> Successive (Min a)
deriving instance (Successive (m a))=> Successive (Alt m a)
deriving instance (Successive (m a))=> Successive (Ap m a)
deriving instance (Ord1 m, Ord1 n, Ord a, Successive (m (n a)))=> Successive (Compose m n a)
deriving instance (Successive a)=> Successive (Const a b)

instance Successive Bool where
  uncheckedDec _ = False
  uncheckedInc _ = True

instance Successive Word where
  uncheckedDec x = x - 1
  uncheckedInc = (+) 1

instance Successive Int where
  uncheckedDec x = x - 1
  uncheckedInc = (+) 1

instance Successive Natural where
  isMax _ = False
  isMin = (Just 0 ==) . GHC.naturalToWordMaybe -- See note on naturalToWordMaybe.

instance Successive Integer where
  isMax _ = False
  isMin _ = False

instance Successive (Fixed a) where
  isMax _ = False
  isMin _ = False

{- |
'Down' reverses the operations:

@
isMax (Down x) = isMin x
isMin (Down x) = isMax x
uncheckedDec (Down x) = Down (uncheckedInc x)
uncheckedInc (Down x) = Down (uncheckedDec x)
@
-}
instance (Successive a)=> Successive (Down a) where
  isMax (Down x) = isMin x
  isMin (Down x) = isMax x
  uncheckedDec (Down x) = Down $ uncheckedInc x
  uncheckedInc (Down x) = Down $ uncheckedDec x


--- Helpers (not exported)

iterateMaybe :: (a -> Maybe a) -> a -> NonEmpty.NonEmpty a
{- ^
@iterateMaybe f x@ is the 'NonEmpty.NonEmpty' list produced by iterating @f@ from @x@ until @f x'@ is @Nothing@.
-}
iterateMaybe f = NonEmpty.unfoldr (\ x -> (x, f x))
{-# INLINE iterateMaybe #-}


{- NOTE: inlining

Most derived methods are marked INLINABLE to allow them to specialize. This has essentialy the same as making them (non-derived) methods and always using the default definition.
`decFrom`, `incFrom`, `decFromTo`, and `incFromTo` are marked INLINABLE to allow fusion. (All are good producers.)

By 'derived methods' I mean functions that are defined in terms of class methods and carry class constraints, not class methods that can be automatically derived.
-}

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

{- NOTE: naturalToWordMaybe

GHC does not unwrap the 0 in '(0 :: Natural) =='. This leads to code blowup:

'(Just 0 ==) . Natural.naturalToWordMaybe' compiles to this core (cleaned up):
\ (x :: Natural) ->
  case x of {
    NatS# w# ->
      case w# of {
        __DEFAULT -> False;
        0## -> True
      };
    NatJ# dt -> False
  }


'(0 ==)' compiles to this core:
\ (ds :: Natural) ->
  case 0 of {
    NatS# a1_a4vn ->
      case ds of {
        NatS# b1_a4vq ->
          tagToEnum#
            @ Bool (eqWord# a1_a4vn b1_a4vq);
        NatJ# ipv_a4vF -> False
      };
    GHC.Natural.NatJ# dt_a4vs ->
      case ds of {
        GHC.Natural.NatS# ipv_a4vH -> False;
        GHC.Natural.NatJ# dt1_a4vw ->
          let {
            nx#_s4Bg :: GHC.Integer.Type.GmpSize#
            [LclId]
            nx#_s4Bg
              = uncheckedIShiftRL#
                  (sizeofByteArray# dt_a4vs) 3# } in
          case ==#
                  nx#_s4Bg
                  (uncheckedIShiftRL#
                    (sizeofByteArray# dt1_a4vw) 3#)
          of {
            __DEFAULT -> False;
            1# ->
              case {__pkg_ccall integer-gmp-1.0.3.0 ByteArray#
                              -> ByteArray#
                              -> Int#
                              -> State# RealWorld
                              -> (# State# RealWorld, Int# #)}_a4vz
                      dt_a4vs dt1_a4vw nx#_s4Bg realWorld#
              of
              { (# ds2_a4vB, ds3_a4vC #) ->
              case ds3_a4vC of {
                __DEFAULT -> False;
                0# -> True
              }
              }
          }
      }
  }
-}
