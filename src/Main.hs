{-# LANGUAGE ScopedTypeVariables #-}
 
module Main (main, censorQueryString) where

import Data.List.Split (splitOn)
import Data.List (isInfixOf, intercalate)

-- utm_content=main&a=5&token=SOMETOKEN
-- utm_content=main&api_token=CENSORED&a=5&token=CENSORED
sampleQS :: String
sampleQS = "utm_content=main&b&a=5&token=SOMETOKEN"

main :: IO ()
main = putStrLn "hello world"

data QueryParam =
    Pear String String
  | Single String
  deriving (Show, Eq)

parseQueryParam :: [String] -> Maybe QueryParam
parseQueryParam (x:y:[]) = Just $ Pear x y
parseQueryParam (x:[])   = Just $ Single x
parseQueryParam _ = Nothing

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
       case maybeParsedQueryParams of 
           Nothing ->
             Nothing
           (Just listParams) ->
            Just $ joinToQueryParam $ (map (convertToString . censor) listParams)
    where
       splitString :: [String]
       splitString = splitOn "&" queryString

       maybeParsedQueryParams :: Maybe [QueryParam]
       maybeParsedQueryParams = traverse (parseQueryParam . (splitOn "=")) splitString