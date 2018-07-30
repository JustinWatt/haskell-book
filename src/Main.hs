{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, censorQueryString) where

import           Data.Function   ((&))
import           Data.List       (group, intercalate, isInfixOf, sort)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import Data.Monoid ((<>))

-- utm_content=main&a=5&token=SOMETOKEN
-- utm_content=main&api_token=CENSORED&a=5&token=CENSORED
sampleQS :: String
sampleQS = "utm_content=main&b&a=5&token=SOMETOKEN"

data QueryParam =
    Pear String String
  | Single String
  deriving (Show, Eq)

parseQueryParam :: [String] -> Maybe QueryParam
parseQueryParam (x:y:[]) = Just $ Pear x y
parseQueryParam (x:[])   = Just $ Single x
parseQueryParam _        = Nothing

censor :: QueryParam -> QueryParam
censor (Pear k v) =
     if (isInfixOf "token" k)
     then (Pear k "CENSORED")
     else (Pear k v)
censor s = s

convertToString :: QueryParam -> String
convertToString (Pear k v) = k ++ "=" ++ v
convertToString (Single k) = k

joinToQueryParam :: [String] -> String
joinToQueryParam = intercalate "&"

censorQueryString :: String -> Maybe String
censorQueryString queryString =
  joinToQueryParam . map (convertToString . censor) <$> maybeParsedQueryParams
  where
    splitString :: [String]
    splitString = splitOn "&" queryString

    maybeParsedQueryParams :: Maybe [QueryParam]
    maybeParsedQueryParams = traverse (parseQueryParam . (splitOn "=")) splitString

vowels :: Set Char
vowels = Set.fromList ['a', 'e', 'i', 'o', 'u']

isVowel :: Char -> Bool
isVowel = (flip Set.member) vowels

vowelCount :: String -> [(Char, Int)]
vowelCount s =
  filter isVowel s &
  group . sort &
  map (\xs -> (head xs, length xs))

parseVowelCount :: (Char, Int) -> String
parseVowelCount (char, count) = show count <> [' ', char]

vowelCountIO :: IO ()
vowelCountIO = do
  line <- getLine
  putStr $ "There are " <> (vowelListToString . vowelCount) line
  putStrLn $ " in the word " <> "'" <> line <> "'"
  return ()
  where
    vowelListToString :: [(Char, Int)] -> String
    vowelListToString [] = "no vowels"
    vowelListToString xs = intercalate ", " $ parseVowelCount <$> xs

main :: IO ()
main = vowelCountIO
