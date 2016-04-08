module Main where

import System.Environment
import Text.ParserCombinators.ReadP

data Token = Token
           deriving (Eq, Ord, Show, Read)

validToken :: ReadP Token
validToken = string "!$!" >> return Token

token :: ReadP (Maybe Token)
token = (Just <$> validToken) <++ (get >> return Nothing)

main :: IO ()
main = do
  input <- (head <$> getArgs) >>= readFile
  print $ (fst <$> (readP_to_S validToken input))
  -- print $ take 5 $ fst $ head $ readP_to_S (many token) input
  return ()
