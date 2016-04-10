{-# language GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.Hashable
import Data.HashMap.Lazy hiding (size, foldr)
import Data.List hiding (insert, delete)
import Data.Prefix.Units
import Data.Word
import System.Environment
import Text.ParserCombinators.ReadP as P
import Text.Read.Lex

newtype Address = Address Word64
                deriving (Eq, Ord, Show, Read, Hashable)

newtype Size = Size Word64
             deriving (Eq, Ord, Show, Read, Num, Real)

instance RationalConvertible Size where
  convFromRational = Size . fromInteger . convFromRational

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
token = validToken <++ (P.get >> token)

tokenList :: String -> [Token]
tokenList input = case readP_to_S token input of
  [(x, rest)] -> x:tokenList rest
  [] -> []
  _ -> error "parse error"

type AllocationState = HashMap Address Size

update :: Token -> State AllocationState ()
update (Allocate a s) = modify $ insert a s
update (Deallocate a) = modify $ delete a

main :: IO ()
main = do
  input <- (head <$> getArgs) >>= readFile
  -- input <- getContents
  let
    tokens = tokenList input
    state = execState (mapM update tokens) mempty
    sizes = elems state
    totalSize = sum sizes
    bySize = sortOn (\(_, _, x) -> x) $ (\(x, y) -> (x, y, x * fromIntegral y)) <$>(head &&& length) <$> group (sort $ elems state)
  print (showValue FormatBinary totalSize, totalSize)
  putStr $ unlines $ show <$> bySize
  return ()
