{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Char 
import Data.Ratio

#ifdef DEBUG
import Debug.Trace
import System.IO
#endif

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
                           (num,unit) = BSL.break (isAlpha . e2e) pre
                           (preComm,postComm) = BSL.break ((`elem` ".,") . e2e) num

                           toString = map e2e . BSL.unpack
                           readSafe x = (fst . last) $ (0,"") : reads x

                           a,b,c :: Ratio Integer
                           a = readSafe (toString preComm) % 1
                           b = let l = fromIntegral $ 10 * BSL.length postComm in
                               case l of
                                 0 -> 0
                                 _ -> readSafe (toString postComm) % l
                           c = (transUnit (toString unit)) % 1
                           result = round ((a+b) * c)
                       in 
#ifdef DEBUG
                         traceShow (map round [a,b,c], map toString [line,pre,num,preComm,postComm,unit]) result
#else
                         result
#endif

                       

e2e :: (Enum a, Enum b) => a -> b
e2e = toEnum . fromEnum

main = do
#ifdef DEBUG
  hPutStrLn stderr "DEBUG MODE!"  
#endif
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