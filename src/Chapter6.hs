{-# LANGUAGE InstanceSigs #-}
module Chapter6 where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
 (==) Mon  Mon  = True
 (==) Tue  Tue  = True
 (==) Weds Weds = True
 (==) Thu  Thu  = True
 (==) Fri  Fri  = True
 (==) Sat  Sat  = True
 (==) Sun  Sun  = True
 (==) _ _       = False

instance Eq Date where
  (Date weekday dayOfMonth)
    == (Date weekday' dayOfMonth') =
         weekday == weekday'
      && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (TisAn a) == (TisAn b) =
    a == b

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two a b) == (Two c d) =
    a == b && c == d

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) =
       a == c
    && b == d

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) =
       a == c
    && b == d

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThatOne b) = a == b
  (==) (ThatOne b) (ThisOne a) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) (ThisOne a) (ThisOne b) = a == b

class Numberish a where
  fromNumber :: Integer -> a
  toNumber   :: a -> Integer

newtype Age =
  Age Integer
  deriving (Eq, Show)

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
