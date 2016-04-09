module Main where

import Data.Word
import System.Environment
import Text.ParserCombinators.ReadP
import Text.Read.Lex

newtype Address = Address Word64
                deriving (Eq, Ord, Show, Read)
newtype Size = Size Word64
             deriving (Eq, Ord, Show, Read)

data Token = Allocate Address Size
           | Deallocate Address
           deriving (Eq, Ord, Show, Read)

address :: ReadP Address
address = Address <$> readHexP

size :: ReadP Size
size = Size <$> readHexP

validToken :: ReadP Token
validToken = between (string "!$!") (char '\n') $
  choice [ char '+' >> Allocate <$> address <*> (char ',' >> size)
         , char '-' >> Deallocate <$> address
         ]

token :: ReadP Token
token = validToken <++ (get >> token)

tokenList :: String -> [Token]
tokenList input = case readP_to_S token input of
  [(x, rest)] -> x:tokenList rest
  [] -> []
  _ -> error "parse error"

main :: IO ()
main = do
  input <- (head <$> getArgs) >>= readFile
  -- input <- getContents
  print $ take 100 $ tokenList input
  return ()
