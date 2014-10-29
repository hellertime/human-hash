module Data.Digest.Human where

import           Data.Foldable
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Text                       (Text)
import           Data.Digest.Human.WordList

-- | Split the input 't a' into 'c' chunks of like size, and for each
-- chunk, hash and apply 'f'. Return the collected results.
--
-- Example:
--
-- > let words = ["foo", "bar"] in humanHashBy ((words !!).(`mod` 2)) 2 "test"
-- ["bar", "bar"]
--
humanHashBy :: (Foldable t, Hashable a) => (Int -> b) -> Int -> t a -> [b]
humanHashBy f c = humanHash' . toList
  where
    humanHash' s = go (length s) s
    go k s       = 
        case splitAt (k `quot` c) s of
            ([],b) -> []
            (a,b)  -> (f . hash) a : go k b

-- | Build a human hash of length 'c' from the input 't a' using the included word list.
humanHash :: (Foldable t, Hashable a) => Int -> t a -> [Text]
humanHash = humanHashBy f
  where
    wl = wordListShort
    e  = length wl
    f  = (wl !!) . (flip mod e)
