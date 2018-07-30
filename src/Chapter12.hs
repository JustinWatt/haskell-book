module Chapter12 where

import Data.List
import Data.Maybe

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s


replaceThe :: String -> String
replaceThe s =
  intercalate " " $ map (fromMaybe "a" . notThe) $ words s
