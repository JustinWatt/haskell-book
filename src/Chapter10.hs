module Chapter10 where

import           Data.Time

data DatabaseItem =
    DbString String
  | DbNumber Integer
  | DbDate UTCTime


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f x acc =
      case x of
        (DbDate d) -> d : acc
        _ -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f x acc =
      case x of
        (DbNumber n) -> n : acc
        _ -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb d = (fromIntegral $ sum numbers) / (fromIntegral (length numbers))
  where
    numbers = filterDbNumber d
