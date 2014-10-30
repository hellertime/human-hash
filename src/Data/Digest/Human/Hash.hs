module Data.Digest.Human.Hash (humanHashBy) where

import           Data.Foldable
import           Data.Hashable

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
