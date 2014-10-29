module Data.Digest.Human where

import           Data.Bits.Extras                (log2, w32)
import           Data.Foldable
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Text                       (Text)
import           Data.Digest.Human.WordList

humanHashChunksWith :: (Foldable t, Hashable a) => (Int -> b) -> Int -> t a -> [b]
humanHashChunksWith f c = humanHash' . toList
  where
    humanHash' s = go (length s) s
    go k s       = 
        case splitAt c s of
            ([],b) -> []
            (a,b)  -> (f . hash) a : go k b

humanHash :: (Foldable t, Hashable a) => t a -> [Text]
humanHash = humanHashChunksWith f c
  where
    wl  = wordListShort
    f   = (wl !!) . (flip mod dim)
    dim = length wl
    c   = case log2 $ w32 dim of
            n | dim < 2^n -> n
              | otherwise -> n + 1
