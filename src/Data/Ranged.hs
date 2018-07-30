{-# language
  PolyKinds, KindSignatures, DataKinds, RoleAnnotations, FlexibleInstances,
  TypeOperators, UndecidableInstances, ScopedTypeVariables, TypeApplications,
  MagicHash, MultiParamTypeClasses, TypeFamilies, ConstraintKinds, TypeInType
#-}

{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Data.Ranged where

import Control.Lens.Prism (Prism', prism', Prism, prism)
import Data.Kind (Constraint)
import Data.TypeLits
import Data.TypeNums
import GHC.Exts
import Numeric.Natural
import Test.Inspection

data Decimal = Point TInt Nat

newtype Ranged (n :: Inf k1) (m :: Inf k2) = Ranged { unRanged :: Integer }
  deriving (Show, Eq)
type role Ranged nominal nominal

data Inf a = PosInf | NegInf | I a

type family (+?) (a :: Inf x) (b :: Inf x) :: Inf (ArithK x x) where
  (+?) PosInf PosInf = PosInf
  (+?) NegInf NegInf = NegInf
  (+?) (I a) NegInf = NegInf
  (+?) NegInf (I a) = NegInf
  (+?) (I a) PosInf = PosInf
  (+?) PosInf (I a) = PosInf
  (+?) (I a) (I b) = I (a + b)

type family (-?) (a :: Inf x) (b :: Inf x) :: Inf (ArithK x x) where
  (-?) PosInf PosInf = PosInf
  (-?) NegInf NegInf = NegInf
  (-?) (I a) NegInf = PosInf
  (-?) NegInf (I a) = NegInf
  (-?) (I a) PosInf = NegInf
  (-?) PosInf (I a) = PosInf
  (-?) (I a) (I b) = I (a - b)

type family (*?) (a :: Inf x) (b :: Inf x) :: Inf (ArithK x x) where
  (*?) NegInf NegInf = PosInf
  (*?) NegInf PosInf = NegInf
  (*?) PosInf NegInf = NegInf
  (*?) PosInf PosInf = PosInf
  (*?) NegInf (I (Pos a)) = NegInf
  (*?) NegInf (I (Neg a)) = PosInf
  (*?) NegInf (I a) = NegInf
  (*?) PosInf (I (Pos a)) = PosInf
  (*?) PosInf (I (Neg a)) = NegInf
  (*?) PosInf (I a) = PosInf
  (*?) (I (Pos a)) NegInf = NegInf
  (*?) (I (Neg a)) NegInf = PosInf
  (*?) (I a) NegInf = NegInf
  (*?) (I (Pos a)) PosInf = PosInf
  (*?) (I (Neg a)) PosInf = NegInf
  (*?) (I a) PosInf = PosInf
  (*?) (I a) (I b) = I (a * b)

type family Min (a :: Inf x) (b :: Inf x) where
  Min NegInf a = NegInf
  Min a NegInf = NegInf
  Min PosInf a = a
  Min a PosInf = a
  Min (I a) (I b) = I (IfThenElse (a <=? b) a b)

type family Min4 a b c d where
  Min4 a b c d = Min a (Min b (Min c d))

type family Max (a :: Inf x) (b :: Inf x) where
  Max NegInf a = a
  Max a NegInf = a
  Max PosInf a = PosInf
  Max a PosInf = PosInf
  Max (I a) (I b) = I (IfThenElse (b <=? a) a b)

type family Max4 a b c d where
  Max4 a b c d = Max a (Max b (Max c d))

(.+) :: Ranged n m -> Ranged n' m' -> Ranged (n +? n') (m +? m')
(.+) (Ranged n) (Ranged m) = Ranged (n + m)

(.-) :: Ranged n m -> Ranged n' m' -> Ranged (n -? n') (m -? m')
(.-) (Ranged n) (Ranged m) = Ranged (n + m)

(.*)
  :: Ranged n m
  -> Ranged n' m'
  -> Ranged
       (Min4 (n*?n') (n*?m') (m*?n') (m*?m'))
       (Max4 (n*?n') (n*?m') (m*?n') (m*?m'))
(.*) (Ranged n) (Ranged m) = Ranged (n * m)

type AtLeast (n :: a) = Ranged (I n) (PosInf :: Inf a)
type AtMost (n :: a) = Ranged (NegInf :: Inf a) (I n)
type Between (n :: k1) (m :: k2) = Ranged (I n) (I m)
type Negative = AtMost (Neg 1)
type Natural = AtLeast (Pos 0)
type Positive = AtLeast (Pos 1)

type family IfThenElse (a :: Bool) (b :: x) (c :: x) where
  IfThenElse 'True a b = a
  IfThenElse 'False a b = b

type family LTE n m :: Constraint where
  LTE n m =
    IfThenElse
      (n <=? m)
      (() :: Constraint)
      (TypeError (ShowType n :<>: 'Text " <= " :<>: 'ShowType m :<>: 'Text " ~ False"))

type family GTE n m :: Constraint where
  GTE n m =
    IfThenElse
      (m <=? n)
      (() :: Constraint)
      (TypeError (ShowType n :<>: 'Text " >= " :<>: 'ShowType m :<>: 'Text " ~ False"))

{-# inline castTo #-}
castTo :: forall a s t b. CastNum s t a b => Prism s t a b
castTo = castNum

class CastNum s t a b where
  castNum :: Prism s t a b

instance (KnownInt n, KnownInt m, GTE n m, KnownInt m') => CastNum (AtLeast n) (AtLeast n) (AtLeast m) (AtLeast m') where
  {-# inline castNum #-}
  castNum =
    prism
      (\(Ranged n) -> Ranged n)
      (\a -> maybe (Left a) Right . toAtLeast @m $ unRanged a)

instance (KnownInt n, KnownInt m, KnownInt o, GTE n m, KnownInt m') => CastNum (Between n o) (Between n o) (AtLeast m) (AtLeast m') where
  {-# inline castNum #-}
  castNum =
    prism
      (\(Ranged n) -> Ranged n)
      (\a -> maybe (Left a) Right . toAtLeast @m $ unRanged a)

instance (KnownInt n, KnownInt m, LTE n m, KnownInt m') => CastNum (AtMost n) (AtMost n) (AtMost m) (AtMost m') where
  {-# inline castNum #-}
  castNum =
    prism
      (\(Ranged n) -> Ranged n)
      (\a -> maybe (Left a) Right . toAtMost @m $ unRanged a)

instance (KnownInt n, KnownInt m, KnownInt o, LTE n m, KnownInt m') => CastNum (Between n o) (Between n o) (AtMost m) (AtMost m') where
  {-# inline castNum #-}
  castNum =
    prism
      (\(Ranged n) -> Ranged n)
      (\a -> maybe (Left a) Right . toAtMost @m $ unRanged a)

{-
instance (KnownInt n, KnownInt m, LTE n m) => CastNum (AtMost n) (AtMost m) where
  {-# inline castNum' #-}
  castNum' = prism' (\(Ranged n) -> Ranged n) (toAtMost . unRanged)

instance
  ( KnownInt n, KnownInt m, KnownInt n', KnownInt m'
  , GTE n n', LTE m m'
  )
  => CastNum (Between n m) (Between n' m') where
  {-# inline castNum' #-}
  castNum' = prism' (\(Ranged n) -> Ranged n) (toBetween . unRanged)
-}

class KnownInt n => ToAtLeast n where
  toAtLeast :: Integer -> Maybe (AtLeast n)

instance KnownInt n => ToAtLeast n where
  {-# inline toAtLeast #-}
  toAtLeast n =
    case intVal' @n proxy# of
      nVal
        | n >= nVal -> Just $ Ranged n
        | otherwise -> Nothing

class KnownInt n => ToAtMost n where
  toAtMost :: Integer -> Maybe (AtMost n)

instance KnownInt n => ToAtMost n where
  {-# inline toAtMost #-}
  toAtMost n =
    case intVal' @n proxy# of
      nVal
        | n <= nVal -> Just $ Ranged n
        | otherwise -> Nothing

class (KnownInt n, KnownInt m) => ToBetween n m where
  toBetween :: Integer -> Maybe (Between n m)

instance (KnownInt n, KnownInt m) => ToBetween n m where
  {-# inline toBetween #-}
  toBetween n =
    case intVal' @n proxy# of
      nVal
        | nVal <= n ->
            case intVal' @m proxy# of
              mVal
                | n <= mVal -> Just $ Ranged n
                | otherwise -> Nothing
        | otherwise -> Nothing

{-# noinline test #-}
test :: Maybe (AtLeast 3)
test = toAtLeast 5

{-# noinline test2 #-}
test2 :: Maybe (AtLeast 3)
test2 = toAtLeast 2

{-# noinline test5 #-}
test5 :: Integer -> Maybe (AtMost (Neg 1))
test5 n = toAtMost n

--- being inspection testing ---

{-# noinline testA #-}
testA :: Integer -> Maybe (AtLeast 3)
testA n = toAtLeast n

{-# noinline testA' #-}
testA' :: Integer -> Maybe Integer
testA' n
  | n >= n' = Just n
  | otherwise = Nothing
  where
    {-# noinline n' #-}
    n' :: Integer
    n' = toInteger (3 :: Numeric.Natural.Natural)

inspect ('testA ==- 'testA')

{-# noinline testB #-}
testB :: Integer -> Maybe (Between 4 10)
testB n = toBetween n


{-# noinline testB' #-}
testB' :: Integer -> Maybe Integer
testB' n
  | n' <= n, n <= n'' = Just n
  | otherwise = Nothing
  where
    {-# noinline n' #-}
    n' :: Integer
    n' = toInteger (4 :: Numeric.Natural.Natural)

    {-# noinline n'' #-}
    n'' :: Integer
    n'' = toInteger (10 :: Numeric.Natural.Natural)

inspect ('testB ==- 'testB')


{-# noinline testC #-}
testC :: Integer -> Integer -> Maybe (AtLeast 12)
testC n m = (.*) <$> testA n <*> testB m

{-# noinline testC' #-}
testC' :: Integer -> Integer -> Maybe Integer
testC' n m = (*) <$> fmap unRanged (testA n) <*> fmap unRanged (testB m)

inspect ('testC ==- 'testC')

--- end inspection testing ---

main :: IO ()
main = do
  print test
  print test2
  print $ test5 (-2)
  -- print $ testA 0
  -- print $ testB 5
  -- print $ testB 3
  -- print $ testC 3 3
  -- print $ testC 3 4
