{-# LANGUAGE OverloadedStrings #-}
module Data.Digest.Human (humanHash) where

import           Data.Foldable
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Text                       (Text)
import           System.Random

import           Data.Digest.Human.WordList

-- | Split the input 't a' into 'c' chunks of like size, and for each
-- chunk, hash and apply 'f'. Return the collected results.
--
-- Example:
--
-- > let words = ["foo", "bar"] in humanHashBy (\i -> (words !!).(`mod` 2)) 2 "test"
-- ["bar", "bar"]
--
humanHashBy :: (Foldable t, Hashable a) => (Int -> Int -> b) -> Int -> t a -> [b]
humanHashBy _ 0 = const []
humanHashBy f c = humanHash' . toList
  where
    humanHash' s = go 0 (length s) s
    go i k []    = []
    go i k s     = 
        case splitAt (k `quot` c) s of
            ([],b)  -> (f i . hash) b : []
            (a,b)   -> if length b < c
                           then (f i . hash) (a ++ b) : []
                           else (f i . hash) a : go (i + 1) k b

-- | Build a human hash of length 'c' from the input 't a' using the included word list.
humanHash :: (Foldable t, Hashable a) => Int -> t a -> [Text]
humanHash c = fixup . humanHashBy (bsAtron posNegGuide c) c
  where
    posNegGuide = take c $ randoms $ mkStdGen c
    fixup []             = []
    fixup h@(_:[])       = h
    fixup hhs@(h:hs)
        | c `mod` 2 == 0 = hhs
        | otherwise      = h : (last hs) : init hs

-- | Generate human hash bullshit
bsAtron :: [Int] -> Int -> Int -> Int -> Text
bsAtron rs c i
    | last           = f places
    | i `mod` 2 /= 0 = f nouns
    | r `mod` 2 == 0 = f positive
    | r `mod` 2 /= 0 = f negative
  where
    last = c `mod` 2 /= 0 && (i + 1) == c
    r    = rs !! i
    f w  = (w !!) . (flip mod (length w))
