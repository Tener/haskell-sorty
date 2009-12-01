module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Char 
import Data.Ratio

transUnit :: String -> Integer
transUnit "G" = 1024 * transUnit "M"
transUnit "M" = 1024 * transUnit "K"
transUnit "K" = 1024

transUnit "g" = 1000 * transUnit "m"
transUnit "m" = 1000 * transUnit "k"
transUnit "k" = 1000

transUnit "" = 1

firstColumnSize :: BSL.ByteString -> Integer
firstColumnSize line = let
                           (pre,_) = BSL.break (isSpace . e2e) line
                           (preComma,postComma) = BSL.break ((`elem` ".,") . e2e) pre
                           (preUnit,unit) = BSL.break (isAlpha . e2e) postComma

                           toString = map e2e . BSL.unpack
                           readSafe x = (fst . last) $ (0,"") : reads x

                           a,b,c :: Ratio Integer
                           a = readSafe (toString preComma) % 1
                           b = let l = fromIntegral $ 10 * BSL.length preUnit in
                               case l of
                                 0 -> 0
                                 _ -> readSafe (toString preUnit) % l
                           c = (transUnit (toString unit)) % 1
                           result = round ((a+b) * c)
                       in 
                         result

                       

e2e :: (Enum a, Enum b) => a -> b
e2e = toEnum . fromEnum

main = do
  input <- BSL.getContents 
  mapM_ BSL.putStrLn (sortBy (comparing firstColumnSize) $ BSL.split (e2e '\n') input)

{- TEST:

1
2
3
1,1k
2,1m
3,1g
1,1K
2,1M
3,1G

-}